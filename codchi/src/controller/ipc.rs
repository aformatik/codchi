use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum ServerMsg {
    Ping,
    Quit,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum ServerAnswer {
    Pong,
}
