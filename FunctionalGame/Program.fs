open Game

[<EntryPoint>]
let main argv = 
    use game = new GameMain()
    game.Run()
    0