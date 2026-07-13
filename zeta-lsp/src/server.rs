use anyhow::Result;
use lsp_server::{Connection, Message};
use lsp_types::{
    CompletionOptions, HoverProviderCapability, SaveOptions, SemanticTokensFullOptions,
    SemanticTokensOptions, SemanticTokensServerCapabilities, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    TextDocumentSyncSaveOptions,
};

use crate::{dispatch, state::ServerState};

pub struct Server<'a, 'bump> {
    connection: Connection,
    state: ServerState<'a, 'bump>,
}

impl<'a, 'bump> Server<'a, 'bump>
where
    'bump: 'a,
{
    pub fn new(compiler: zeta_compiler_api::Compiler<'a, 'bump>) -> Result<Self> {
        let (connection, _) = Connection::stdio();

        Ok(Self {
            connection,
            state: ServerState {
                compiler,
                workspace_root: None,
                documents: Default::default(),
                should_exit: false,
            },
        })
    }

    pub fn run(mut self) -> Result<()> {
        let capabilities = serde_json::to_value(ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Options(
                TextDocumentSyncOptions {
                    open_close: Some(true),
                    change: Some(TextDocumentSyncKind::FULL),
                    save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                        include_text: Some(false),
                    })),
                    ..Default::default()
                },
            )),
            document_symbol_provider: Some(lsp_types::OneOf::Left(true)),
            definition_provider: Some(lsp_types::OneOf::Left(true)),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            completion_provider: Some(CompletionOptions {
                trigger_characters: Some(vec![".".to_string(), "::".to_string()]),
                resolve_provider: Some(false), // set true later if we add completionItem/resolve
                ..Default::default()
            }),
            semantic_tokens_provider: Some(
                SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                    legend: crate::semantic_tokens::legend(),
                    full: Some(SemanticTokensFullOptions::Bool(true)),
                    range: None,
                    work_done_progress_options: Default::default(),
                }),
            ),
            rename_provider: Some(lsp_types::OneOf::Right(lsp_types::RenameOptions {
                prepare_provider: Some(false),
                work_done_progress_options: Default::default(),
            })),
            ..Default::default()
        })
        .unwrap();

        let initialize_params = self.connection.initialize(capabilities)?;
        dispatch::initialize(&mut self.state, initialize_params)?;

        for message in &self.connection.receiver {
            match message {
                Message::Request(req) => {
                    if self.connection.handle_shutdown(&req)? {
                        break;
                    }
                    dispatch::request(&self.connection, &mut self.state, &req)?;
                }
                Message::Notification(notification) => {
                    dispatch::notification(&self.connection, &mut self.state, &notification)?;
                    if self.state.should_exit {
                        break;
                    }
                }
                Message::Response(response) => {
                    dispatch::response(&mut self.state, &response)?;
                }
            }
        }
        Ok(())
    }
}
