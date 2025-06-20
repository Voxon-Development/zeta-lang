use crate::ast;
use crate::codegen::ir::ir_compiler::IrCompiler;
use crate::codegen::ir::module::ZetaModule;
use crate::frontend::{self, lexer::Lexer, tokens};
use anyhow::Result;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::fs;
use tokio::sync::{Mutex, MutexGuard};
use tokio::task::JoinHandle;

#[derive(Debug, thiserror::Error)]
pub enum AsyncCompileError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    
    #[error("Lexer error: {0:?}")]
    Lexer(Vec<tokens::TokenError>),
    
    #[error("Parser error: {0}")]
    Parser(String),
}

/// Compiles multiple files asynchronously and returns a single ZetaModule containing all the compiled code.
/// Files are compiled in parallel, and the results are merged into a single module.
pub async fn compile_files_async(files: Vec<PathBuf>) -> Result<ZetaModule, AsyncCompileError> {
    let module = Arc::new(Mutex::new(ZetaModule::new()));
    
    // Create a stream of compilation tasks
    let mut tasks: Vec<JoinHandle<Result<(), AsyncCompileError>>> = Vec::with_capacity(files.len());
    
    for file_path in files {
        let module: Arc<Mutex<ZetaModule>> = module.clone();

        let task: JoinHandle<Result<(), AsyncCompileError>> = tokio::spawn(async move {
            let content: String = fs::read_to_string(&file_path).await.map_err(AsyncCompileError::from)?;

            if let Ok((_, functions, classes)) = process_file(content.as_str()).await {
                let mut lock = module.lock().await;
                let mut compiler = IrCompiler::new(&mut lock);
                compiler.compile(functions, classes);
            }

            Ok(())
        });


        tasks.push(task);
    }
    
    // Wait for all tasks to complete
    for task in tasks {
        if let Err(e) = task.await {
            eprintln!("Task failed: {}", e);
        }
    }
    
    // Extract the final module
    let module_guard = Arc::into_inner(module)
        .expect("Failed to get inner Arc value")
        .into_inner();
    
    Ok(module_guard)
}

/// Processes a single file's content and returns its compilation units
async fn process_file(content: &str) -> Result<(ast::FuncDecl, Vec<ast::FuncDecl>, Vec<ast::ClassDecl>), AsyncCompileError> {
    // Create lexer and tokenize
    let mut lexer = Lexer::new(content.to_string());
    let tokens = lexer.tokenize()
        .map_err(|_| AsyncCompileError::Lexer(lexer.errors))?;
    
    // Parse the tokens
    let stmts = frontend::parser::parse_program(tokens)
        .map_err(|e| AsyncCompileError::Parser(e.to_string()))?;
    
    // Extract functions and classes
    let functions: Vec<_> = stmts.iter()
        .filter_map(|stmt| 
            if let ast::Stmt::FuncDecl(f) = stmt { 
                Some(f.clone()) 
            } else { 
                None 
            })
        .collect();
        
    let main = functions.iter()
        .find(|f| f.name == "main")
        .cloned()
        .ok_or_else(|| 
            AsyncCompileError::Parser("No 'main' function found in file".to_string())
        )?;
        
    let classes: Vec<_> = stmts.iter()
        .filter_map(|stmt| 
            if let ast::Stmt::ClassDecl(c) = stmt { 
                Some(c.clone()) 
            } else { 
                None 
            })
        .collect();
    
    Ok((main, functions, classes))
}
