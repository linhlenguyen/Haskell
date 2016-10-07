module Data(
Character(..),
WorldState(..),
Action(..),
Sprite(..),
SpriteResource
)
  where
    import qualified Graphics.Gloss.Data.Point as GP
    import qualified Graphics.Gloss.Interface.Pure.Game as Game
    import qualified Codec.BMP as Codec
    import qualified Data.Map.Strict as Map

    type SpriteResource = Map.Map Sprite Codec.BMP

    data Action = Stop | MoveLeft | MoveRight | Jump | Crouch
    data Sprite = Stand | MoveLeft1 | MoveLeft2 | MoveLeft3 | MoveRight1 | MoveRight2 | MoveRight3 deriving (Eq,Ord)

    data Character = Character {
      c_position :: GP.Point,
      c_action :: Action,
      c_currentSprite :: Sprite
    }

    data WorldState = WorldState {
      ws_player :: Character,
      ws_keyPressed :: [Game.Key],
      ws_background :: Codec.BMP,
      ws_sprites :: SpriteResource
    }
