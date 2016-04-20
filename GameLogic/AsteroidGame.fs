module AsteroidGame

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

type GunStatus = 
    | Cooldown of float32
    | Ready

type BarSpawnStatus = 
    | Cooldown of float32
    | Ready

type Projectile = 
    {
        Position : Vector2
    }

type Enemy = 
    {
        Position : Vector2
    }

type Ball = 
    {
        Position : Vector2
        Velocity : Vector2
    }

type Bar = 
    {
        Position : Vector2
    }

type Ship = 
    {
        Position : Vector2
        Velocity : Vector2
    }

type GameState =
    {
        Gun             : GunStatus
        Enemies         : List<Enemy>
        Projectiles     : List<Projectile>
        Ship            : Ship
        Ball            : Ball
        Bars            : List<Bar>
        SpawnStatus     : BarSpawnStatus 
    }

//type GamePhase = 
//  | StartMenu 
//  | Playing of GameState
//  | Dead of GameState


    
let updateBall (isColliding:Ball->bool) (ks:KeyboardState, ms:MouseState, dt:float32, ball:Ball) =
    let speed = 1000.0f;
    let ball = 
        if (isColliding ball) then
            {
                ball with Velocity = Vector2(ball.Velocity.X,-50.0f) - Vector2.UnitY * dt * 60.0f
            }
        else if (ball.Position.Y < 400.0f) then
            {
                ball with Velocity = ball.Velocity + Vector2.UnitY * dt * speed
            }
        else 
            ball
            

    let ball = 
        if (ks.IsKeyDown(Keys.A) || ks.IsKeyDown(Keys.Left)) then
            if (ball.Position.X >= 0.0f) then
                {
                    ball with Velocity = ball.Velocity - Vector2.UnitX * speed * dt 
                }
            else 
                {
                     ball with Velocity = Vector2(0.0f, ball.Velocity.Y)
                }
        else 
            ball

    let ball = 
        if (ks.IsKeyDown(Keys.D) || ks.IsKeyDown(Keys.Right)) then
            if (ball.Position.X <= 740.0f) then
                {
                    ball with Velocity = ball.Velocity + Vector2.UnitX * speed * dt 
                }
            else 
                {
                    ball with Velocity = Vector2(0.0f, ball.Velocity.Y)
                }
        else 
            ball
    { 
        ball with 
            Position = ball.Position + ball.Velocity * dt; 
            Velocity = ball.Velocity * 0.95f 
    }

let updateShip (ks:KeyboardState, ms:MouseState, dt:float32, ship:Ship) =
    let speed = 1000.0f;
    let ship =
        if ks.IsKeyDown(Keys.Left) then
            { 
                ship with Velocity = ship.Velocity - Vector2.UnitX * speed * dt 
            }
        else
            ship
    let ship = 
        if ks.IsKeyDown(Keys.Right) then
            { 
                ship with Velocity = ship.Velocity + Vector2.UnitX * speed * dt 
            }
        else
            ship
    let ship =
        if ks.IsKeyDown(Keys.Down) then
            { 
                ship with Velocity = ship.Velocity + Vector2.UnitY * speed * dt 
            }
        else
            ship
    let ship = 
        if ks.IsKeyDown(Keys.Up) then
            { 
                ship with Velocity = ship.Velocity - Vector2.UnitY * speed * dt 
            }
        else
            ship
    { 
        ship with 
            Position = ship.Position + ship.Velocity * dt; 
            Velocity = ship.Velocity * 0.9f 
    }
        
let randomBar() =
    {
        Bar.Position = Vector2(float32(r.Next(0, 700)), 460.0f)
    }

let updateBar(dt:float32) (bar:Bar) : Bar =
    {
        bar with Position = bar.Position - Vector2.UnitY * dt * 50.0f
    }

let spawnRandomBars(amount:int, bars:List<Bar>) : List<Bar> =  
    let a = randomBar() << bars
    let b = randomBar() << a
    let c = randomBar() << b
    let d = randomBar() << c
    let e = randomBar() << d
    let f = randomBar() << e
    let g = randomBar() << f
    let h = randomBar() << g
    h
    (*let bars = 
        for i in 1 .. amount do
            randomBar() << bars
    bars*)
    

let updateBars (dt:float32) (bars:List<Bar>) (spawnStatus:BarSpawnStatus) =
    let bars = 
        if (spawnStatus = BarSpawnStatus.Ready) then
            spawnRandomBars(r.Next(0,5), bars)
        else 
            bars
    let bars = map (updateBar dt) bars
    let insideScreen (e:Bar) : bool =
        e.Position.Y > 0.0f
    filter insideScreen bars

let randomEnemy() =
    {
        Enemy.Position = Vector2(float32(r.Next(0, 700)), 0.0f)
    }

let updateEnemy (dt:float32) (enemy:Enemy) : Enemy =
    {
        enemy with Position = enemy.Position + Vector2.UnitY * dt * 10.0f
    }

let updateEnemies (isNotHit:Enemy->bool) (dt:float32) (enemies:List<Enemy>) =
    let creationProbability() =
        let l = length enemies
        if l < 5 then
            100
        elif l < 15 then
            20
        else
            5
    let enemies = 
        if r.Next(0,100) < creationProbability() then
            randomEnemy() << enemies
        else
            enemies
    let enemies = map (updateEnemy dt) enemies
    let insideScreen (e:Enemy) : bool =
        e.Position.Y < 600.0f
    let enemies = filter insideScreen enemies
    filter isNotHit enemies

let updateProjectile (dt:float32) (projectile:Projectile) : Projectile =
    {
        projectile with Position = projectile.Position - Vector2.UnitY * dt * 100.0f
    }

let updateProjectiles (newProjectile:Unit->Projectile) (shouldCreateNow:Unit->bool) (dt:float32) (projectiles:List<Projectile>) =
    let projectiles = 
        if shouldCreateNow() then
            newProjectile() << projectiles
        else
            projectiles
    let projectiles = map (updateProjectile dt) projectiles
    let insideScreen (p:Projectile) : bool =
        p.Position.Y > -100.0f
    filter insideScreen projectiles



type Drawable = 
    {
        Position : Vector2
        Image    : string
    }

let drawProjectile (projectile:Projectile) : Drawable =
    {
        Drawable.Position = projectile.Position
        Drawable.Image    = "laser.png"
    }

let drawEnemy (enemy:Enemy) : Drawable =
    {
        Drawable.Position = enemy.Position
        Drawable.Image    = "enemy.png"
    }

let drawBar (bar:Bar) : Drawable = 
    {
        Drawable.Position = bar.Position
        Drawable.Image    = "bar.png"
    }





let initialState() = 
    {
        Gun         = GunStatus.Ready
        Enemies     = Empty
        Projectiles = Empty
        Ship = 
            {
                Position = Vector2(320.0f, 400.0f)
                Velocity = Vector2.Zero
            }
        Ball = 
            {
                Position = Vector2(320.0f, 0.0f)
                Velocity = Vector2.Zero
            }
        Bars        = Empty
        SpawnStatus = BarSpawnStatus.Ready
    }



let updateState (ks:KeyboardState) (ms:MouseState) (dt:float32) (gameState:GameState) =
    let spawnState = 
        match gameState.SpawnStatus with
        | BarSpawnStatus.Ready ->
            BarSpawnStatus.Cooldown(2.0f)
        | BarSpawnStatus.Cooldown t ->
            if (t < 0.0f) then 
                BarSpawnStatus.Ready
            else 
                BarSpawnStatus.Cooldown(t - dt)
        


    let isShootingNow,newGun = 
        match gameState.Gun with
        | GunStatus.Ready ->
            if ks.IsKeyDown(Keys.Space) then
                (fun () -> true), GunStatus.Cooldown 0.2f
            else
                (fun () -> false), GunStatus.Ready
        | GunStatus.Cooldown t ->
            if t > 0.0f then
                (fun () -> false), GunStatus.Cooldown(t-dt)
            else
                (fun () -> false), GunStatus.Ready

    let createProjectileAtShip() =
        {
            Projectile.Position = gameState.Ship.Position
        }

    let rec isBallColliding (bars:List<Bar>) (ball:Ball) = 
        match bars with
        | Empty -> false
        | Node(p,ps) ->
            let a = Rectangle(int p.Position.X, int p.Position.Y, 72, 16)
            let b = Rectangle(int ball.Position.X, int ball.Position.Y, 60, 60)
            if (a.Intersects(b)) then             //if Vector2.Distance(p.Position, ball.Position) < 25.0f then
                true
            else 
                isBallColliding ps ball

    let rec isEnemyNotHit (projectiles:List<Projectile>) (e:Enemy) =
        match projectiles with
        | Empty -> true
        | Node(p,ps) ->
            if Vector2.Distance(p.Position, e.Position) < 120.0f then
                false
            else
                isEnemyNotHit ps e

    { 
        gameState with 
                Ship         = updateShip(ks, ms, dt, gameState.Ship)
                Enemies      = updateEnemies (isEnemyNotHit gameState.Projectiles) dt gameState.Enemies
                Projectiles  = updateProjectiles createProjectileAtShip isShootingNow dt gameState.Projectiles
                Gun          = newGun
                Bars         = updateBars dt gameState.Bars gameState.SpawnStatus
                Ball         = updateBall (isBallColliding gameState.Bars) (ks, ms, dt, gameState.Ball)
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

    (*let listOfDrawableProjectiles =
        map drawProjectile gameState.Projectiles |> toFSharpList
    let listOfDrawableEnemies =
        map drawEnemy gameState.Enemies |> toFSharpList
    [
        {
            Drawable.Position = gameState.Ship.Position
            Drawable.Image    = "spaceShip.png"
        }
    ] @ listOfDrawableEnemies @ listOfDrawableProjectiles
        |> Seq.ofList*)
