module Data.UI.Input.Keyboard.ModKey(ModKey(..), unModKey, pretty) where

import Data.Monoid(mempty)
import Data.UI.Input.Keyboard.Mods(ModsState)
import qualified Data.UI.Input.Keyboard.Mods as Mods
import Data.UI.Input.Keyboard.Keys(Key)
import qualified Data.UI.Input.Keyboard.Keys as Keys

data ModKey = ModKey ModsState Key
  deriving (Show, Read, Eq, Ord)

unModKey :: ModKey -> (ModsState, Key)
unModKey (ModKey x y) = (x, y)

pretty :: ModKey -> String
pretty (ModKey modsState key) =
  (if modsState == mempty
   then ""
   else Mods.pretty modsState ++ "-") ++
  Keys.pretty key
