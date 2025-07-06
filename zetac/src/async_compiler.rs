use crate::ast;
use crate::codegen::ir::ir_compiler::{ClassTable, IrCompiler};
use crate::codegen::ir::module::ZetaModule;
use crate::frontend::{self, lexer::Lexer, tokens};
use anyhow::Result;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::fs;
use tokio::sync::Mutex;
use tokio::task::JoinHandle;
use ir::bump::{AtomicBump, Bump};
use crate::ast::{ClassDecl, FuncDecl};
use crate::frontend::tokens::Token;

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
pub async fn compile_files_async(files: Vec<PathBuf>) -> Result<(ZetaModule, ClassTable), AsyncCompileError> {
    // Validate files before checking them
    let invalid_files: Vec<String> = files.iter()
        .filter_map(|path| if path.exists() { 
            None 
        } else { 
            Some(path.file_name().unwrap().to_string_lossy().to_string()) 
        })
        .collect::<Vec<String>>();

    if !invalid_files.is_empty() {
        return Err(AsyncCompileError::Io(std::io::Error::new(
            std::io::ErrorKind::NotFound, 
            format!("Files not found: {:?}", invalid_files)
        )));
    }
    
    let class_table = Arc::new(Mutex::new(ClassTable::new()));

    let module = Arc::new(Mutex::new(ZetaModule::new()));
    
    // Create a stream of compilation tasks
    let mut tasks: Vec<JoinHandle<Result<(), AsyncCompileError>>, Bump> = Vec::with_capacity_in(files.len(), Bump::new());
    
    for file_path in files {
        let module: Arc<Mutex<ZetaModule>> = module.clone();
        let class_table: Arc<Mutex<ClassTable>> = class_table.clone();

        let task: JoinHandle<Result<(), AsyncCompileError>> = tokio::spawn(async move {
            let content: String = fs::read_to_string(file_path).await.map_err(AsyncCompileError::from)?;
            if let Ok((_, functions, classes)) = 
                    process_file(content.as_str()).await {
                let mut lock = module.lock().await;
                let mut compiler = IrCompiler::new(&mut lock);
                compiler.compile(functions, classes);

                let mut lock = class_table.lock().await;
                lock.compressed.layouts.extend(compiler.classes.compressed.layouts);
            }

            Ok(())
        });


        tasks.push(task);
    }
    
    let mut failed = false;
    // Wait for all tasks to complete
    for task in tasks {
        match task.await {
            Err(e) => {
                eprintln!("Task failed: {}", e);
                failed = true;
            },
            _ => {}
        }
    }
    
    if failed {
        return Err(AsyncCompileError::Io(std::io::Error::new(std::io::ErrorKind::Other, "Compilation failed")));
    }
    
    // Extract the final module
    let module_guard = Arc::into_inner(module)
        .expect("Failed to get inner Arc value")
        .into_inner();
    
    let class_table_guard = Arc::into_inner(class_table)
        .expect("Failed to get inner Arc value")
        .into_inner();
    
    Ok((module_guard, class_table_guard))
}

/// Processes a single file's content and returns its compilation units
async fn process_file(content: &str) -> Result<(FuncDecl, Vec<FuncDecl>, Vec<ClassDecl>), AsyncCompileError> {
    // Create lexer and tokenize
    let mut lexer = Lexer::new(content.to_string());
    let tokens: Vec<Token, AtomicBump> = lexer.tokenize()
        .map_err(|_| AsyncCompileError::Lexer(lexer.errors))?;

    println!("Tokens: {:#?}", tokens);
    
    // Parse the tokens
    let stmts: Vec<ast::Stmt, AtomicBump> = frontend::parser::parse_program(tokens)
        .map_err(|e| AsyncCompileError::Parser(e.to_string()))?;

    println!("AST: {:#?}", stmts);
    
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
        
    let classes: Vec<ClassDecl> = stmts.iter()
        .filter_map(|stmt| 
            if let ast::Stmt::ClassDecl(c) = stmt { 
                Some(c.clone()) 
            } else { 
                None 
            })
        .collect();
    
    Ok((main, functions, classes))
}
