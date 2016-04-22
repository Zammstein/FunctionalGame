using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Input;
using Microsoft.Xna.Framework.Graphics;

namespace MainApplication {
    class Game : Microsoft.Xna.Framework.Game {
        SpriteBatch spriteBatch;
        SpriteFont font;
        GraphicsDeviceManager graphics;
        BallGame.GameState gameState;

        public Game() {
            graphics = new GraphicsDeviceManager(this);
            Content.RootDirectory = "Content";
        }

        protected override void LoadContent() {
            spriteBatch = new SpriteBatch(GraphicsDevice);
            font = Content.Load<SpriteFont>("Font");
            gameState = BallGame.initialState();
            base.LoadContent();
        }

        protected override void Update(GameTime gameTime) {
            gameState = BallGame.updateState(
                Keyboard.GetState(), Mouse.GetState(), 
                (float)gameTime.ElapsedGameTime.TotalSeconds, 
                gameState);
            base.Update(gameTime);
        }

        protected override void Draw(GameTime gameTime) {
            GraphicsDevice.Clear(Color.CornflowerBlue);

            spriteBatch.Begin();
            foreach (var drawable in BallGame.drawState(gameState)) {
                spriteBatch.Draw(Content.Load<Texture2D>(drawable.Image),
                drawable.Position, Color.White);
            }
            foreach (var hudString in BallGame.drawHud(gameState)) {
                spriteBatch.DrawString(font, hudString.Text, hudString.Position, Color.Black);
            }
            spriteBatch.End();

            base.Draw(gameTime);
        }
    }

    class Program {
        static void Main(string[] args) {
            using (var game = new Game()) {
            game.Run();
        }
    }
  }
}
