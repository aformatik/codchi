Describe "codchi.msix" {
   It "installs codchistore correctly" {
       # Disable VcXsrv & tray icon (don't work in headless mode)
       $configDir = "$env:APPDATA\codchi"
       New-Item -ItemType Directory $configDir
       "vcxsrv.enable = false
       tray.autostart = false" | Out-File -FilePath "$configDir\config.toml" -Encoding utf8
       wsl.exe --shutdown
       { codchi.exe status -vv } | Should -Not -Throw 
   }
   It "initializes a base machine" {
       { codchi.exe -vv init base } | Should -Not -Throw 
       codchi.exe exec -vv base env | Should -Contain "CODCHI_MACHINE_NAME=base"
   }
   It "clones and initializes a code machine for codchi development" {
       { codchi.exe -vv clone codchi https://github.com/aformatik/codchi nixosModules.codchi } | Should -Not -Throw 
   }
   It "builds codchi inside codchi" {
       { codchi.exe exec -vv codchi bash -lc 'cd codchi/codchi && direnv allow && cargo run -- --version' } | Should -Not -Throw
   }
}
