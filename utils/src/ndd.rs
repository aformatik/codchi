use std::io::{self, BufRead, BufReader, Read, Write};
use std::process::{exit, Child, Command, Stdio};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};
use std::{env, thread};
use sysinfo::{CpuRefreshKind, Networks, RefreshKind, System};

/// Nix dont deadlock
/// A wrapper for nix which monitors it for deadlocks (CPU & network usage). If a deadlock is
/// detected, the nix child command is restarted (this is ok because nix will continue with the
/// build where it left)

const NIX_LOG_ERR: u64 = 0;
// const NIX_LOG_DEBUG: u64 = 3;
const NIX_LOG_TRACE: u64 = 4;

// fake nix log message
fn log(level: u64, msg: &str) {
    eprintln!(r#"@nix {{ "action": "msg", "level": {level}, "msg": "ndd> {msg}" }}"#)
}

fn main() {
    let debug = std::env::var("CODCHI_DEBUG").is_ok();
    let args: Vec<String> = env::args().skip(1).collect();
    let stdout_last_activity = Arc::new(Mutex::new(Instant::now()));
    let mut child = start_nix_process(&args, stdout_last_activity.clone());

    let check_interval = Duration::from_secs(1);
    let max_inactive_duration = Duration::from_secs(15);

    let mut sys = System::new_with_specifics(
        RefreshKind::new().with_cpu(CpuRefreshKind::new().with_cpu_usage()),
    );
    let mut net = Networks::new_with_refreshed_list();
    let mut last_activity = Instant::now();

    loop {
        sys.refresh_cpu();
        net.refresh();

        if let Some(status) = child
            .try_wait()
            .expect("Failed while attempting to wait for nix...")
        {
            log(
                NIX_LOG_TRACE,
                &format!("Nix terminated with exit code {status}"),
            );
            exit(status.code().unwrap_or(1))
        } else {
            let cpu_usage = sys.global_cpu_info().cpu_usage();
            let network_usage_kbs = net.iter().map(|(_, net)| net.received()).sum::<u64>() / 1_000; // 1 KB
            let stdout_inactivity =
                Instant::now().duration_since(*stdout_last_activity.lock().unwrap());

            let inactivity = Instant::now().duration_since(last_activity);

            if debug {
                log(
                    NIX_LOG_TRACE,
                    &format!(
                        "Nix considered inactive for {inactivity:?}. \
CPU: {cpu_usage}, Network: {network_usage_kbs}, Stdout/err: {stdout_inactivity:?}"
                    ),
                );
            }

            if cpu_usage > 2.0
                || network_usage_kbs > 1
                || stdout_inactivity <= max_inactive_duration
            {
                last_activity = Instant::now();
            } else if inactivity > max_inactive_duration {
                log(
                    NIX_LOG_ERR,
                    "Detected deadlock. Deleting locks and restarting nix...",
                );
                child.kill().expect("Failed to kill nix process");
                Command::new("bash")
                    .args(["-c", "rm -f /nix/store/*.lock"])
                    .spawn()
                    .expect("Failed spawning the ndd lock remover.")
                    .wait()
                    .expect("Error while removing nix's locks");
                child = start_nix_process(&args, stdout_last_activity.clone());
                last_activity = Instant::now();
            }
        }
        std::thread::sleep(check_interval);
    }
}

fn start_nix_process(args: &[String], stdout_last_activity: Arc<Mutex<Instant>>) -> Child {
    let mut child = Command::new("nix")
        .args(args)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to start nix...");

    let child_out = std::mem::take(&mut child.stdout).expect("cannot attach to child stdout");
    let child_err = std::mem::take(&mut child.stderr).expect("cannot attach to child stderr");

    monitor_std_activity(stdout_last_activity.clone(), child_out, io::stdout());
    monitor_std_activity(stdout_last_activity.clone(), child_err, io::stderr());

    child
}

fn monitor_std_activity(
    last_activity: Arc<Mutex<Instant>>,
    input: impl Read + Send + 'static,
    mut output: impl Write + Send + 'static,
) {
    thread::spawn(move || {
        let reader = BufReader::new(input);
        let mut iter = reader.lines();
        while let Some(Ok(line)) = iter.next() {
            *last_activity.lock().unwrap() = Instant::now();
            writeln!(output, "{line}").expect("Failed writing stdout!");
        }
    });
}
