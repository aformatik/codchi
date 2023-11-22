export def "main hello" [
    name: string
] {
    echo $"Hello ($name) from nu"
}


export def "main native" [name: string] {
    ^hello.exe $name
}
