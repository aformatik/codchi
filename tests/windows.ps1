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
   It "initializes a code machine for codchi development" {
       wsl.exe --shutdown
       { codchi.exe -vv init codchi https://github.com/aformatik/codchi nixosModules.base } | Should -Not -Throw 
   }
   It "opens the codchi code machine" {
       wsl.exe --shutdown
       codchi.exe exec -vv codchi env | Should -Contain "CODCHI_MACHINE_NAME=codchi"
   }
}
