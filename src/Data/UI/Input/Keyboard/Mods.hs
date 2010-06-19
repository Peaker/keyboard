module Data.UI.Input.Keyboard.Mods(ModState(..), ModsState(..), simpleShowModState, pretty) where

import Data.List(intercalate)
import Data.Monoid(Monoid(..))

data ModState = ModState {
  anyPressed :: Bool,           -- We may know that at least one of
                                -- them is pressed without knowing
                                -- which
  leftPressed :: Bool,
  rightPressed :: Bool
  }
  deriving (Show, Read, Eq, Ord)

instance Monoid ModState where
  mempty = ModState False False False
  a `mappend` b = ModState (anyPressed a || anyPressed b)
                           (leftPressed a || leftPressed b)
                           (rightPressed a || rightPressed b)

data ModsState = ModsState {
  shift :: ModState,
  ctrl :: ModState,
  alt :: ModState,
  meta :: ModState,
  numLock :: Bool,
  capsLock :: Bool
  }
  deriving (Show, Read, Eq, Ord)

instance Monoid ModsState where
  mempty = ModsState mempty mempty mempty mempty False False
  a `mappend` b = ModsState (shift a `mappend` shift b)
                            (ctrl a `mappend` ctrl b)
                            (alt a `mappend` alt b)
                            (meta a `mappend` meta b)
                            (numLock a || numLock b)
                            (capsLock a || capsLock b)

simpleShowModState :: String -> ModState -> String
simpleShowModState str modState =
  case modState of
    ModState True  True  True  -> "Both" ++ str
    ModState True  True  False -> "L" ++ str
    ModState True  False True  -> "R" ++ str
    ModState True  False False -> str
    ModState False False False -> ""
    ModState False _     _     -> error "Invalid ModState"

pretty :: ModsState -> String
pretty modsState =
  ("+" `intercalate`
   concatMap (uncurry showModState) mods) ++
  if null lockStrs then "" else
    "(" ++
    ("," `intercalate` lockStrs) ++
    ")"
  where
    mods = [("Shift", shift),
            ("Ctrl", ctrl),
            ("Alt", alt),
            ("Meta", meta)]
    locks = [("NumLock", numLock),
             ("CapsLock", capsLock)]
    lockStrs = concatMap (uncurry showLock) locks
    showLock name lock = if lock modsState then [name] else []
    showModState name m =
      if m modsState == mempty then [] else [
        simpleShowModState name (m modsState)
        ]
