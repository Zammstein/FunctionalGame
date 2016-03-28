module Entity

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input


[<AbstractClass>]
type Entity(position:Vector2, image:string) = 
    member this.Position = position
    member this.Image = image
    member this.Velocity = velocity

    // virtual method(s):
    abstract member UpdateState : KeyboardState * MouseState * float32 * Entity -> Entity
    default this.UpdateState (ks:KeyboardState, ms:MouseState, dt:float32, entity:Entity) = entity

    abstract member Move : Vector2 * float32 * Entity-> Entity
    default this.Move (dir:Vector2, dt:float32, entity:Entity) = 
        entity
        //entity with Position = this.Position + Vector2.UnitY * dt * 10.0f

let Move (ks:KeyboardState, dt:float32, entity:Entity) : Entity = 
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



