use anyhow::{bail, Result};
use windows::Win32::{
    Foundation::{GetLastError, LocalFree, BOOL, HLOCAL},
    System::Diagnostics::Debug::{
        FormatMessageW, FORMAT_MESSAGE_ALLOCATE_BUFFER, FORMAT_MESSAGE_FROM_STRING,
        FORMAT_MESSAGE_IGNORE_INSERTS,
    },
};

pub trait WindowsResult {
    fn to_result(self) -> Result<()>;
}

impl WindowsResult for BOOL {
    fn to_result(self) -> Result<()> {
        if !self.as_bool() {
            let mut buffer: *mut u16 = std::ptr::null_mut();
            unsafe {
                let error_code = GetLastError().0;
                let size = FormatMessageW(
                    FORMAT_MESSAGE_ALLOCATE_BUFFER
                        | FORMAT_MESSAGE_FROM_STRING
                        | FORMAT_MESSAGE_IGNORE_INSERTS,
                    None,
                    error_code,
                    0,
                    std::mem::transmute(&mut buffer),
                    0,
                    None,
                );
                if size == 0 {
                    bail!("No error found for error code {error_code}");
                }
                let message =
                    String::from_utf16_lossy(std::slice::from_raw_parts(buffer, size as usize));
                LocalFree(HLOCAL(buffer as *mut _));
                bail!(message)
            }
        }
        Ok(())
    }
}
