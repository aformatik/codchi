use crate::data::*;

trait CodchiDriver {
    fn get_status() -> CodchiStatus;
    fn healthcheck() -> CodchiHealth;
}
