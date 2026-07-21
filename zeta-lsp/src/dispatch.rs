#![allow(unused)]
use anyhow::{Context, Result};
use ir::ast::Stmt;
use ir::hir::{HirType, StrId};
use ir::span::SourceSpan;
use lsp_server::{Connection, Message, Notification, Request, RequestId, Response};
use lsp_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, DidSaveTextDocument, Exit,
    Notification as _, PublishDiagnostics,
};
use lsp_types::request::{
    Completion, DocumentSymbolRequest, GotoDefinition, GotoTypeDefinitionParams, HoverRequest,
    References, Rename, Request as _,
};
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse,
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DidSaveTextDocumentParams, DocumentSymbol, DocumentSymbolParams, DocumentSymbolResponse,
    GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverContents, HoverParams, Location,
    MarkupContent, MarkupKind, Position, PublishDiagnosticsParams, ReferenceParams, RenameParams,
    SymbolKind, TextDocumentContentChangeEvent, TextEdit, Uri, WorkspaceEdit,
};
use sentinel_typechecker::type_checker::SymbolId;
use std::collections::HashMap;
use std::path::PathBuf;
use std::str::FromStr;

use crate::lsp_diagnostics::group_by_file;
use crate::state::{Document, ServerState};
use crate::text_utils::{
    byte_offset_in_source, declaration_span, ident_at_column, ident_at_end, line_at,
    nearest_prior_occurrence, prefix_before, span_to_range, tightest_occurrence, to_span_coords,
};
use crate::urls::{path_to_uri, uri_to_path};

pub fn initialize(state: &mut ServerState, params: serde_json::Value) -> Result<()> {
    if let Ok(init) = serde_json::from_value::<lsp_types::InitializeParams>(params) {
        state.workspace_root = init
            .workspace_folders
            .as_ref()
            .and_then(|folders| folders.first())
            .and_then(|folder| uri_to_path(&folder.uri).ok())
            .or_else(|| {
                #[allow(deprecated)]
                init.root_uri.as_ref().and_then(|uri| uri_to_path(uri).ok())
            });
    }
    Ok(())
}

pub fn request(connection: &Connection, state: &mut ServerState, request: &Request) -> Result<()> {
    match request.method.as_str() {
        HoverRequest::METHOD => {
            let params: HoverParams = serde_json::from_value(request.params.clone())?;
            let result: Option<Hover> = hover_at(state, &params);
            let response = Response::new_ok(request.id.clone(), result);
            connection.sender.send(Message::Response(response))?;
        }
        DocumentSymbolRequest::METHOD => {
            let params: DocumentSymbolParams = serde_json::from_value(request.params.clone())?;
            let result = document_symbols_at(state, &params);
            let response = Response::new_ok(request.id.clone(), result);
            connection.sender.send(Message::Response(response))?;
        }
        Completion::METHOD => {
            let params: CompletionParams = serde_json::from_value(request.params.clone())?;
            let items = completions_at(state, &params);
            let response =
                Response::new_ok(request.id.clone(), Some(CompletionResponse::Array(items)));
            connection.sender.send(Message::Response(response))?;
        }
        GotoDefinition::METHOD => {
            let params: GotoDefinitionParams = serde_json::from_value(request.params.clone())?;
            let result = go_to_definition(state, &params);
            let response = Response::new_ok(request.id.clone(), result);
            connection.sender.send(Message::Response(response))?;
        }
        References::METHOD => {
            let params: ReferenceParams = serde_json::from_value(request.params.clone())?;
            let result = references_at(state, &params);
            let response = Response::new_ok(request.id.clone(), result);
            connection.sender.send(Message::Response(response))?;
        }
        Rename::METHOD => {
            let params: RenameParams = serde_json::from_value(request.params.clone())?;
            match rename_at(state, &params) {
                Ok(edit) => {
                    connection.sender.send(Message::Response(Response::new_ok(
                        request.id.clone(),
                        edit,
                    )))?;
                }
                Err(msg) => {
                    connection.sender.send(Message::Response(Response::new_err(
                        request.id.clone(),
                        -32803, // LSP "RequestFailed"
                        msg,
                    )))?;
                }
            }
        }
        _ => {
            let response = Response::new_err(
                request.id.clone(),
                lsp_server::ErrorCode::MethodNotFound as i32,
                format!("unhandled method: {}", request.method),
            );
            connection.sender.send(Message::Response(response))?;
        }
    }
    Ok(())
}

pub fn notification(
    connection: &Connection,
    state: &mut ServerState,
    notification: &Notification,
) -> Result<()> {
    match notification.method.as_str() {
        DidOpenTextDocument::METHOD => {
            let params: DidOpenTextDocumentParams =
                serde_json::from_value(notification.params.clone())?;
            let uri = params.text_document.uri;
            let path =
                uri_to_path(&uri).map_err(|_| anyhow::anyhow!("non-file URI: {}", uri.as_str()))?;

            state.documents.insert(
                uri.clone(),
                Document {
                    version: params.text_document.version,
                    text: params.text_document.text.clone(),
                    path: path.clone(),
                },
            );

            let reporter = state.compiler.open_module(&path, params.text_document.text);
            publish_all(connection, &reporter)?;
        }

        DidChangeTextDocument::METHOD => {
            let params: DidChangeTextDocumentParams =
                serde_json::from_value(notification.params.clone())?;
            let uri = params.text_document.uri;

            let Some(doc) = state.documents.get_mut(&uri) else {
                return Ok(());
            };
            doc.version = params.text_document.version;
            let path = doc.path.clone();

            for change in params.content_changes {
                apply_change(&mut doc.text, change);
            }

            let reporter = state.compiler.update_module(&path, doc.text.clone());
            publish_all(connection, &reporter)?;
        }

        DidSaveTextDocument::METHOD => {
            let params: DidSaveTextDocumentParams =
                serde_json::from_value(notification.params.clone())?;
            let uri = params.text_document.uri;
            let Some(doc) = state.documents.get(&uri) else {
                return Ok(());
            };
            let path = doc.path.clone();
            // include_text: false, so re-read from disk explicitly here.
            let source = std::fs::read_to_string(&path).unwrap_or_default();
            let reporter = state.compiler.update_module(&path, source);
            publish_all(connection, &reporter)?;
        }

        DidCloseTextDocument::METHOD => {
            let params: DidCloseTextDocumentParams =
                serde_json::from_value(notification.params.clone())?;
            let uri = params.text_document.uri;
            if let Some(doc) = state.documents.remove(&uri) {
                if let Some(stem) = doc.path.file_stem().and_then(|s| s.to_str()) {
                    state.compiler.close_module(stem);
                }
            }
        }

        Exit::METHOD => {
            state.should_exit = true;
        }

        _ => {}
    }
    Ok(())
}

pub fn response(state: &mut ServerState, response: &Response) -> Result<()> {
    Ok(())
}

/// Publishes one publishDiagnostics notification per file touched by this
/// reporter, converting each file_name string back to a file:// URI.
fn publish_all(
    connection: &Connection,
    reporter: &ir::errors::reporter::ErrorReporter,
) -> Result<()> {
    for (file_name, diagnostics) in group_by_file(reporter) {
        let Ok(uri) = lsp_types::Uri::from_str(file_name.as_str()) else {
            continue;
        };
        let params = PublishDiagnosticsParams {
            uri,
            diagnostics,
            version: None,
        };
        let notif = Notification::new(PublishDiagnostics::METHOD.to_string(), params);
        connection.sender.send(Message::Notification(notif))?;
    }
    Ok(())
}

fn rename_at(state: &ServerState, params: &RenameParams) -> Result<WorkspaceEdit, String> {
    let text_document = &params.text_document_position.text_document;
    let position = params.text_document_position.position;
    let new_name = &params.new_name;

    let doc = state
        .documents
        .get(&text_document.uri)
        .ok_or("document not open")?;
    let module_idx = state
        .compiler
        .module_idx_for_path(&doc.path)
        .ok_or("module not loaded")?;
    let source = state
        .compiler
        .source_text(module_idx)
        .ok_or("source unavailable")?;
    let (line, column) = to_span_coords(&source, position);

    let checker_rc = state.compiler.type_checker();
    let checker = checker_rc.borrow();

    let (_, _, symbol_id) = tightest_occurrence(checker.occurrences(), module_idx, line, column)
        .ok_or("no renameable symbol at this position")?;

    if matches!(symbol_id, SymbolId::Item { .. }) {
        return Err(
            "renaming functions, structs, enums, and interfaces isn't supported yet, \
                    usage sites for those aren't tracked, only declarations, so renaming \
                     would break every call site silently"
                .to_string(),
        );
    }

    let mut by_module: HashMap<usize, Vec<(SourceSpan, StrId)>> = HashMap::default();
    for (span, name, _, m, sid, _) in checker.occurrences() {
        if *sid == symbol_id {
            by_module.entry(*m).or_default().push((*span, *name));
        }
    }
    if by_module.is_empty() {
        return Err("symbol has no recorded occurrences".to_string());
    }

    let mut changes: HashMap<lsp_types::Uri, Vec<TextEdit>> = HashMap::default();

    for (m, entries) in by_module {
        let m_source = state
            .compiler
            .source_text(m)
            .ok_or("source unavailable for a module containing this symbol")?;
        let m_path = state
            .compiler
            .path_for_module(m)
            .ok_or("no file path recorded for a module containing this symbol")?;

        let mut edits = Vec::new();
        for (span, name) in entries {
            let expected = name.to_string();
            if crate::text_utils::span_text(&m_source, &span).as_deref() != Some(expected.as_str())
            {
                return Err(format!(
                    "rename aborted: couldn't safely locate `{}` at one of its recorded \
                     positions. This happens when a symbol has at least one usage whose span \
                     covers more than just the identifier itself (a known gap for field access \
                     expressions), renaming it isn't safe until that's fixed upstream.",
                    expected
                ));
            }
            let range = span_to_range(
                &m_source,
                span.line,
                span.column,
                span.end_line,
                span.end_column,
            );
            edits.push(TextEdit {
                range,
                new_text: new_name.clone(),
            });
        }

        let uri = path_to_uri(m_path).map_err(|_| "failed to build file URI".to_string())?;
        changes.insert(uri, edits);
    }

    Ok(WorkspaceEdit {
        changes: Some(changes),
        document_changes: None,
        change_annotations: None,
    })
}

fn completions_at(state: &ServerState, params: &CompletionParams) -> Vec<CompletionItem> {
    let uri = &params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;

    let Some(doc) = state.documents.get(uri) else {
        return vec![];
    };
    let Some(module_idx) = state.compiler.module_idx_for_path(&doc.path) else {
        return vec![];
    };
    let Some(source) = state.compiler.source_text(module_idx) else {
        return vec![];
    };

    let prefix = prefix_before(&source, position);

    let checker_rc = state.compiler.type_checker();
    let checker = checker_rc.borrow();
    let ctx = checker.context();

    if let Some(dot_idx) = prefix.rfind('.') {
        let base = ident_at_end(&prefix[..dot_idx]);
        if !base.is_empty() {
            // `base` is either a type name (static access) or a local
            // variable (instance access via last-known type before cursor).
            let struct_name: Option<String> = if ctx.structs.contains_key(base) {
                Some(base.to_string())
            } else {
                let (line, column) = to_span_coords(&source, position);
                nearest_prior_occurrence(checker.occurrences(), module_idx, base, line, column)
                    .and_then(|ty| match strip_container(&ty) {
                        HirType::Struct { name, .. } => Some(name.to_string()),
                        _ => None,
                    })
            };

            let mut items = Vec::new();
            if let Some(struct_name) = struct_name {
                if let Some(def) = ctx.structs.get(&struct_name) {
                    for field in def.fields {
                        items.push(CompletionItem {
                            label: field.name.to_string(),
                            kind: Some(CompletionItemKind::FIELD),
                            detail: Some(field.field_type.to_string()),
                            ..Default::default()
                        });
                    }
                }
                if let Some(table) = ctx.type_methods.get(&struct_name) {
                    for (name, func) in &table.methods {
                        let ret = func
                            .return_type
                            .map(|t| t.to_string())
                            .unwrap_or_else(|| "void".to_string());
                        items.push(CompletionItem {
                            label: name.clone(),
                            kind: Some(CompletionItemKind::METHOD),
                            detail: Some(format!("() -> {}", ret)),
                            ..Default::default()
                        });
                    }
                }
            }
            return items;
        }
    }

    // Otherwise: global symbol completion (this module + its direct imports).
    let mut items = Vec::new();

    for name in ctx.structs.keys() {
        items.push(CompletionItem {
            label: name.clone(),
            kind: Some(CompletionItemKind::STRUCT),
            ..Default::default()
        });
    }
    for name in ctx.enums.keys() {
        items.push(CompletionItem {
            label: name.clone(),
            kind: Some(CompletionItemKind::ENUM),
            ..Default::default()
        });
    }
    for name in ctx.interfaces.keys() {
        items.push(CompletionItem {
            label: name.clone(),
            kind: Some(CompletionItemKind::INTERFACE),
            ..Default::default()
        });
    }
    if let Some(funcs) = ctx.module_functions.get(&module_idx) {
        for (name, func) in funcs {
            let ret = func
                .return_type
                .map(|t| t.to_string())
                .unwrap_or_else(|| "void".to_string());
            items.push(CompletionItem {
                label: name.clone(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some(ret),
                ..Default::default()
            });
        }
    }
    for imp_idx in state
        .compiler
        .dep_graph_ref()
        .borrow()
        .get_module_imports(module_idx)
    {
        if let Some(funcs) = ctx.module_functions.get(&imp_idx) {
            for name in funcs.keys() {
                items.push(CompletionItem {
                    label: name.clone(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    ..Default::default()
                });
            }
        }
    }

    items
}

fn hover_at(state: &ServerState, params: &HoverParams) -> Option<Hover> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    let doc = state.documents.get(uri)?;
    let module_idx = state.compiler.module_idx_for_path(&doc.path)?;
    let source = state.compiler.source_text(module_idx)?;
    let (line, column) = to_span_coords(&source, position);
    let word = ident_at_column(line_at(&source, position.line)?, position.character)?;

    let checker_rc = state.compiler.type_checker();
    let checker = checker_rc.borrow();
    let ctx = checker.context();

    if let Some((occ_name, ty, _)) =
        tightest_occurrence(checker.occurrences(), module_idx, line, column)
    {
        if occ_name.to_string() == word {
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!("```zeta\n{}: {}\n```", word, ty),
                }),
                range: None,
            });
        }
    }

    let value = if let Some(def) = ctx.structs.get(word) {
        format!(
            "```zeta\nstruct {} {{ {} field(s) }}\n```",
            word,
            def.fields.len()
        )
    } else if ctx.enums.get(word).is_some() {
        format!("```zeta\nenum {}\n```", word)
    } else if ctx.interfaces.get(word).is_some() {
        format!("```zeta\ninterface {}\n```", word)
    } else if let Some(func) = ctx
        .module_functions
        .get(&module_idx)
        .and_then(|f| f.get(word))
    {
        let ret = func
            .return_type
            .map(|t| t.to_string())
            .unwrap_or_else(|| "void".to_string());
        let argc = func.params.map(|p| p.len()).unwrap_or(0);
        format!("```zeta\nfn {}({} param(s)) -> {}\n```", word, argc, ret)
    } else {
        return None;
    };

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value,
        }),
        range: None,
    })
}

fn strip_container<'a, 'b, 'bump>(ty: &'b HirType<'a, 'bump>) -> &'b HirType<'a, 'bump> {
    match ty {
        HirType::Ref { inner, .. } => strip_container(inner),
        HirType::SafePointer(inner) => strip_container(inner),
        HirType::UnsafePointer(inner) => strip_container(inner),
        HirType::OwnedPointer(inner) => strip_container(inner),
        _ => ty,
    }
}

fn document_symbols_at(
    state: &ServerState,
    params: &DocumentSymbolParams,
) -> Option<DocumentSymbolResponse> {
    let uri = &params.text_document.uri;
    let doc = state.documents.get(uri)?;
    let module_idx = state.compiler.module_idx_for_path(&doc.path)?;
    let source = state.compiler.source_text(module_idx)?;
    let stmts = state.compiler.ast_stmts(module_idx)?;

    let mut symbols = Vec::new();
    for stmt in stmts {
        let (name, kind, span) = match stmt {
            Stmt::FuncDecl(f) => (f.name.to_string(), SymbolKind::FUNCTION, f.span),
            Stmt::StructDecl(s) => (s.name.to_string(), SymbolKind::STRUCT, s.span),
            Stmt::EnumDecl(e) => (e.name.to_string(), SymbolKind::ENUM, e.span),
            Stmt::InterfaceDecl(i) => (i.name.to_string(), SymbolKind::INTERFACE, i.span),
            _ => continue,
        };

        let range = span_to_range(
            &source,
            span.line,
            span.column,
            span.end_line,
            span.end_column,
        );

        #[allow(deprecated)] // DocumentSymbol.deprecated is a required-shape field we don't use
        symbols.push(DocumentSymbol {
            name,
            detail: None,
            kind,
            tags: None,
            deprecated: None,
            range,
            selection_range: range,
            children: None,
        });
    }

    Some(DocumentSymbolResponse::Nested(symbols))
}

fn item_decl_span<'a, 'bump>(
    stmts: &'bump [Stmt<'a, 'bump>],
    item_idx: usize,
    tag: &str,
) -> Option<SourceSpan<'a>> {
    let stmt = stmts.get(item_idx)?;
    match (stmt, tag) {
        (Stmt::FuncDecl(f), "func_sig" | "func_body") => Some(f.span),
        (Stmt::StructDecl(s), "type") => Some(s.span),
        (Stmt::EnumDecl(e), "type") => Some(e.span),
        (Stmt::InterfaceDecl(i), "trait") => Some(i.span),
        _ => None,
    }
}

fn go_to_definition(
    state: &mut ServerState,
    params: &GotoDefinitionParams,
) -> Option<GotoDefinitionResponse> {
    let uri: &Uri = &params.text_document_position_params.text_document.uri;
    let position: Position = params.text_document_position_params.position;

    let doc = state.documents.get(uri)?;
    let module_idx = state.compiler.module_idx_for_path(&doc.path)?;
    let source = state.compiler.source_text(module_idx)?;
    let (line, column) = to_span_coords(&source, position);

    let checker_rc = state.compiler.type_checker();
    let checker = checker_rc.borrow();

    let (occ_name, ty, symbol_id) =
        tightest_occurrence(checker.occurrences(), module_idx, line, column)?;

    let (target_module, span) = match symbol_id {
        SymbolId::Local(_) | SymbolId::Field { .. } => {
            let span = declaration_span(checker.occurrences(), symbol_id)?;
            (module_idx, span)
        }
        SymbolId::Item {
            module_idx: m,
            item_idx,
            tag,
        } => {
            let stmts = state.compiler.ast_stmts(m)?;
            let span = item_decl_span(stmts, item_idx, tag)?;
            (m, span)
        }
    };

    let target_path = state.compiler.path_for_module(target_module)?;
    let target_source = state.compiler.source_text(target_module)?;
    let range = span_to_range(
        &target_source,
        span.line,
        span.column,
        span.end_line,
        span.end_column,
    );
    let target_uri = path_to_uri(target_path).ok()?;

    Some(GotoDefinitionResponse::Scalar(Location {
        uri: target_uri,
        range,
    }))
}

fn references_at(state: &ServerState, params: &ReferenceParams) -> Vec<Location> {
    let text_document = &params.text_document_position.text_document;
    let position = params.text_document_position.position;
    let include_declaration = params.context.include_declaration;

    let Some(doc) = state.documents.get(&text_document.uri) else {
        return vec![];
    };
    let Some(module_idx) = state.compiler.module_idx_for_path(&doc.path) else {
        return vec![];
    };
    let Some(source) = state.compiler.source_text(module_idx) else {
        return vec![];
    };
    let (line, column) = to_span_coords(&source, position);

    let checker_rc = state.compiler.type_checker();
    let checker = checker_rc.borrow();

    let Some((_, _, symbol_id)) =
        tightest_occurrence(checker.occurrences(), module_idx, line, column)
    else {
        return vec![];
    };

    if let SymbolId::Item {
        module_idx: m,
        item_idx,
        tag,
    } = symbol_id
    {
        if !include_declaration {
            return vec![];
        }
        let Some(stmts) = state.compiler.ast_stmts(m) else {
            return vec![];
        };
        let Some(span) = item_decl_span(stmts, item_idx, tag) else {
            return vec![];
        };
        let Some(target_path) = state.compiler.path_for_module(m) else {
            return vec![];
        };
        let Some(target_source) = state.compiler.source_text(m) else {
            return vec![];
        };
        let range = span_to_range(
            &target_source,
            span.line,
            span.column,
            span.end_line,
            span.end_column,
        );
        let Ok(uri) = path_to_uri(target_path) else {
            return vec![];
        };
        return vec![Location { uri, range }];
    }

    let mut locations = Vec::new();
    for (span, _, _, m, sid, is_decl) in checker.occurrences() {
        if *sid != symbol_id {
            continue;
        }
        if *is_decl && !include_declaration {
            continue;
        }
        let Some(m_source) = state.compiler.source_text(*m) else {
            continue;
        };
        let Some(m_path) = state.compiler.path_for_module(*m) else {
            continue;
        };
        let range = span_to_range(
            &m_source,
            span.line,
            span.column,
            span.end_line,
            span.end_column,
        );
        let Ok(uri) = path_to_uri(m_path) else {
            continue;
        };
        locations.push(Location { uri, range });
    }

    locations
}

pub fn apply_change(source: &mut String, change: TextDocumentContentChangeEvent) {
    if let Some(range) = change.range {
        let start = byte_offset_in_source(source, range.start);
        let end = byte_offset_in_source(source, range.end);

        source.replace_range(start..end, &change.text);
    } else {
        // Client sent the whole document.
        *source = change.text;
    }
}
