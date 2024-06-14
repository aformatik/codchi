Describe "codchi.msix" {
   It "installs codchistore correctly" {
       wsl.exe --shutdown
       codchi.exe status -vv | Should -Not -Throw 
   }
}
