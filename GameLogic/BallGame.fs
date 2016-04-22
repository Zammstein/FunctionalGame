module BallGame

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

let r = new System.Random()

type List<'a> = 
    | Empty 
    | Node of 'a * List<'a>

let (<<) x xs = Node(x,xs)

let rec toFSharpList l =
    match l with
    | Empty -> []
    | Node(x,xs) -> x :: toFSharpList xs

let rec filter (p:'a->bool) (l:List<'a>) : List<'a> =
    match l with
    | Empty -> Empty
    | Node(x,xs) ->
        if p x then
            Node(x, filter p xs)
        else
            filter p xs

let rec map (f:'a->'b) (l:List<'a>) : List<'b> =
    match l with
    | Empty -> Empty
    | Node(x:'a,xs:List<'a>) -> 
        let y:'b = f x
        let ys:List<'b> = map f xs
        Node(y,ys)
 
let rec length (l:List<'a>) =
    match l with
    | Empty -> 0
    | Node(x,xs) -> 1 + length xs

type BarSpawnStatus = 
    | Cooldown of float32
    | Ready

type Ball = 
    {
        Position : Vector2
        Velocity : Vector2
    }

type Bar = 
    {
        Position : Vector2
    }

type CurrentScore = 
    {
        Position : Vector2
        Score    : float32
    }

type GameState =
    {
        Ball            : Ball
        Bars            : List<Bar>
        SpawnStatus     : BarSpawnStatus 
        Score           : CurrentScore
        Paused          : bool
    }

//type GamePhase = 
//      | StartMenu 
//      | Playing of GameState
//      | Dead of GameState
    
let updateBall (isColliding:Ball->bool) (ks:KeyboardState, ms:MouseState, dt:float32, ball:Ball) =
    let speed = 1000.0f;

    let ball = 
        if (isColliding ball) then
            {
                ball with Velocity = Vector2(ball.Velocity.X,-50.0f) - Vector2.UnitY * dt * 60.0f
            }
        else if (ball.Position.Y < 430.0f) then
            {
                ball with Velocity = ball.Velocity + Vector2.UnitY * dt * speed
            }
        else 
            {
                ball with Velocity = Vector2(ball.Velocity.X, 0.0f)
            }
            

    let ball = 
        if (ks.IsKeyDown(Keys.A) || ks.IsKeyDown(Keys.Left)) then
            if (ball.Position.X < 0.0f) then
                {
                    ball with 
                        Velocity = ball.Velocity - Vector2.UnitX * speed * dt 
                        Position = Vector2(800.0f ,ball.Position.Y)
                }
            else
                {
                    ball with Velocity = ball.Velocity - Vector2.UnitX * speed * dt 
                } 
        else 
            ball

    let ball = 
        if (ks.IsKeyDown(Keys.D) || ks.IsKeyDown(Keys.Right)) then
            if (ball.Position.X > 800.0f) then
                {
                    ball with 
                        Velocity = ball.Velocity + Vector2.UnitX * speed * dt
                        Position = Vector2(0.0f ,ball.Position.Y) 
                }
            else 
                {
                    ball with Velocity = ball.Velocity + Vector2.UnitX * speed * dt 
                }
        else 
            ball
    { 
        ball with 
            Position = ball.Position + ball.Velocity * dt; 
            Velocity = ball.Velocity * 0.95f 
    }
        
let randomBar(amount:int) =
    {
        Bar.Position = Vector2(80.0f * (float32)amount, 480.0f)
    }

let updateBar(dt:float32) (bar:Bar) : Bar =
    {
        bar with Position = bar.Position - Vector2.UnitY * dt * 50.0f
    }

let rec spawnRandomBars(amount:int, skip:int, bars:List<Bar>) : List<Bar> = 
    if (amount = skip) then
        spawnRandomBars(amount - 1, skip, bars)
    else if (amount >= 0) then
        spawnRandomBars(amount - 1, skip, randomBar(amount) << bars)
    else 
        bars

let updateBars (dt:float32) (bars:List<Bar>) (spawnStatus:BarSpawnStatus) =
    let bars = 
        if (spawnStatus = BarSpawnStatus.Ready) then
            spawnRandomBars(10, r.Next(0, 10), bars)
        else 
            bars
    let bars = map (updateBar dt) bars
    let insideScreen (e:Bar) : bool =
        e.Position.Y > -16.0f
    filter insideScreen bars

let updateScore (dt:float32) (cs : CurrentScore) =
    let updatedScore = 
        cs.Score + dt
    let cs = 
        {
            cs with
                Score     = updatedScore
                Position  = cs.Position
        }
    cs

type Drawable = 
    {
        Position : Vector2
        Image    : string
    }

type HudString = 
    {
        Position : Vector2
        Text     : string
    }

let drawPlayerScore (score: CurrentScore) : HudString =
    {
        HudString.Position = score.Position
        HudString.Text     = string score.Score
    }

let drawBar (bar:Bar) : Drawable = 
    {
        Drawable.Position = bar.Position
        Drawable.Image    = "bar.png"
    }

let initialState() = 
    {
        Ball = 
            {
                Position = Vector2(320.0f, 0.0f)
                Velocity = Vector2.Zero
            }
        Bars        = Empty
        SpawnStatus = BarSpawnStatus.Ready
        Score = 
            {
                Position = Vector2(50.0f, 50.0f)
                Score    = 0.0f
            }
        Paused = false
    }



let updateState (ks:KeyboardState) (ms:MouseState) (dt:float32) (gameState:GameState) =

    let updatePlayState = 
        if (ks.IsKeyDown(Keys.P)) then 
            not gameState.Paused
        else 
            gameState.Paused
        
    let spawnState = 
        if not gameState.Paused then
            match gameState.SpawnStatus with
            | BarSpawnStatus.Ready ->
                BarSpawnStatus.Cooldown(2.0f)
            | BarSpawnStatus.Cooldown t ->
                if (t < 0.0f) then 
                    BarSpawnStatus.Ready
                else 
                    BarSpawnStatus.Cooldown(t - dt)
         else 
            gameState.SpawnStatus
     
    let rec isBallColliding (bars:List<Bar>) (ball:Ball) = 
        match bars with
        | Empty -> false
        | Node(p,ps) ->
            let barRec = Rectangle(int p.Position.X, int p.Position.Y, 80, 16)
            let ballRec = Rectangle(int ball.Position.X, int ball.Position.Y, 50, 50)
            if (barRec.Intersects(ballRec)) then            
                true
            else 
                isBallColliding ps ball

    { 
        gameState with 
                Paused       = updatePlayState  
                Score        = 
                    if not gameState.Paused then
                        updateScore dt gameState.Score
                    else 
                        gameState.Score
                Bars         = 
                    if not gameState.Paused then
                        updateBars dt gameState.Bars gameState.SpawnStatus
                    else 
                        gameState.Bars
                Ball         = 
                    if not gameState.Paused then
                        updateBall (isBallColliding gameState.Bars) (ks, ms, dt, gameState.Ball)
                    else 
                        gameState.Ball
                SpawnStatus  = spawnState
    }


let drawState (gameState:GameState) : seq<Drawable> =
    let listOfDrawableBars = 
        map drawBar gameState.Bars |> toFSharpList
 
    [
        {
            Drawable.Position = gameState.Ball.Position
            Drawable.Image    = "ball.png"
        }
    ] @ listOfDrawableBars
        |> Seq.ofList

let drawHud(gameState:GameState) : seq<HudString> = 
    [
        {
            HudString.Position = gameState.Score.Position
            HudString.Text     = string (System.Math.Floor((float)gameState.Score.Score))
        }
    ] 
        |> Seq.ofList

