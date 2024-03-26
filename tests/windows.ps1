Describe "codchi.msix" {
   It "installs codchistore correctly" {
       codchi.exe status -vvv | Should -Not -Throw 
   }
}
