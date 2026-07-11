use anyhow::Result;
use lsp_server::{Connection, Message};

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
            },
        })
    }

    pub fn run(mut self) -> Result<()> {
        let capabilities = serde_json::json!({
            // I'll fill these in later
        });

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
                }

                Message::Response(response) => {
                    dispatch::response(&mut self.state, &response)?;
                }
            }
        }

        Ok(())
    }
}
