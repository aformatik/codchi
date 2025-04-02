// #[tarpc::service]
// pub trait Main {
//     /// Returns a greeting for name.
//     async fn hello(name: String) -> String;
// }

use remoc::prelude::*;

pub type RtcResult<T> = Result<T, rtc::CallError>;
// type MainResult<T> = Result<T, IncreaseError>;

pub type Healthcheck = Result<(), String>;

/// Remote counting service.
#[rtc::remote]
pub trait Api {
    async fn ping(&mut self) -> RtcResult<Healthcheck>;
    // ping / health check
    // status
    // get machine config

    // async fn value(&self) -> RtcResult<u32>;
    // async fn watch(&mut self) -> RtcResult<rch::watch::Receiver<u32>>;
    // async fn increase(&mut self, by: u32) -> RtcResult<()>;
    // async fn count_to_value( &self, step: u32, delay: Duration,) -> RtcResult<rch::mpsc::Receiver<u32>>;
}
