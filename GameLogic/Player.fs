module Player

open Entity
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

let texture = "zombie.png"

type Player(position:Vector2, speed:float32) =
    inherit Entity(position, texture)
    member this.Speed = speed

    override this.UpdateState (ks:KeyboardState, ms:MouseState, dt:float32, entity:Entity) : Entity = 
        printfn "update player"
        