module Data(
Character(..),
WorldState(..),
Action(..),
Sprite(..)
)
  where
    import qualified Graphics.Gloss.Data.Point as GP
    import qualified Graphics.Gloss.Interface.Pure.Game as Game
    import qualified Codec.BMP as Codec

    data Character = Character {
      c_position :: GP.Point,
      c_action :: Action,
      c_currentSprite :: Sprite,
      c_sprites :: [(Sprite, Codec.BMP)]
    }

    data WorldState = WorldState {
      ws_player :: Character,
      ws_keyPressed :: [Game.Key],
      ws_background :: Codec.BMP
    }

    data Action = Stop | MoveLeft | MoveRight | Jump | Crouch
    data Sprite = Stand | MoveLeft1 | MoveLeft2 | MoveLeft3 | MoveRight1 | MoveRight2 | MoveRight3 deriving (Eq)
