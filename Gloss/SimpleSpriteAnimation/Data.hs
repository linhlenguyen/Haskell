module Data(
Character(..),
WorldState(..),
Action(..),
Sprite(..),
SpriteResource
)
  where
    import qualified Graphics.Gloss.Data.Picture as Gloss
    import qualified Graphics.Gloss.Data.Point as Gloss
    import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
    import qualified Data.Map.Lazy as Map

    type SpriteResource = Map.Map Sprite Gloss.Picture

    data Action = Stop | MoveLeft | MoveRight | Jump | Crouch
    data Sprite = Stand | MoveLeft1 | MoveLeft2 | MoveLeft3 | MoveRight1 | MoveRight2 | MoveRight3 | Background deriving (Eq,Ord)

    data Character = Character {
      c_position :: Gloss.Point,
      c_action :: Action,
      c_currentSprite :: Sprite
    }

    data WorldState = WorldState {
      ws_player :: Character,
      ws_keyPressed :: [Gloss.Key]
    }
