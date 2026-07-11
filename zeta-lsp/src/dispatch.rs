use anyhow::Result;

use lsp_server::{Connection, Notification, Request, Response};

use crate::state::ServerState;

pub fn initialize(state: &mut ServerState, params: serde_json::Value) -> Result<()> {
    Ok(())
}

pub fn request(connection: &Connection, state: &mut ServerState, request: &Request) -> Result<()> {
    Ok(())
}

pub fn notification(
    connection: &Connection,
    state: &mut ServerState,
    notification: &Notification,
) -> Result<()> {
    Ok(())
}

pub fn response(state: &mut ServerState, response: &Response) -> Result<()> {
    Ok(())
}
