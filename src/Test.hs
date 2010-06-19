import Data.UI.Input.Keyboard.ModKey(ModKey(..), pretty)

import Data.Monoid(mempty)
import Data.UI.Input.Keyboard.Mods(ModState(..), ModsState(..))
import qualified Data.UI.Input.Keyboard.Mods as Mods
import qualified Data.UI.Input.Keyboard.Keys as Keys

main :: IO ()
main = do
  print . pretty $ ModKey (mempty {shift=mempty{anyPressed=True}}) Keys.Escape
  print . pretty $ ModKey (mempty {shift=mempty{anyPressed=True, leftPressed=True}})
    Keys.Escape
  print . pretty $ ModKey (mempty {shift=mempty{anyPressed=True, leftPressed=True, rightPressed=True}})
    Keys.Escape
  print . pretty $ ModKey (mempty {ctrl=mempty{anyPressed=True}, shift=mempty{anyPressed=True}})
    (Keys.Letter 'a')
  print . pretty $ ModKey (mempty {ctrl=mempty{anyPressed=True},
                                   shift=mempty{anyPressed=True},
                                   numLock=True, capsLock=True})
    (Keys.Letter 'a')
