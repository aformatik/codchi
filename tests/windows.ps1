Describe "codchi.msix" {
   It "installs codchistore correctly" {
       wsl.exe --shutdown
       { codchi.exe status -vv } | Should -Not -Throw 
   }
   It "initializes a code machine for codchi development" {
       { codchi.exe init codchi https://github.com/aformatik/codchi nixosModules.codchi } | Should -Not -Throw 
   }
   It "opens the codchi code machine" {
       $configPath = "$env:APPDATA\codchi\config.toml"
       "vcxsrv.enable = false" | Out-File -FilePath $configPath -Encoding utf8
       codchi.exe exec codchi env | Should -Contain "CODCHI_MACHINE_NAME=codchi"
   }
}
