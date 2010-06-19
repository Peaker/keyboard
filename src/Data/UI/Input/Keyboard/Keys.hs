module Data.UI.Input.Keyboard.Keys(Key(..), pretty) where

data Key =
    Backspace
  | Tab
  | BackTab
  | Clear
  | Return
  | Pause
  | Escape
  | Space
  | Exclaim
  | DoubleQuote
  | Hash
  | Dollar
  | Ampersand
  | Quote
  | LeftParen
  | RightParen
  | Asterisk
  | Plus
  | Comma
  | Minus
  | Period
  | Slash
  | Digit Int
  | Colon
  | Semicolon
  | Less
  | Equals
  | Greater
  | QuestionMark
  | At
  | LeftBracket
  | Backslash
  | RightBracket
  | Caret
  | Underscore
  | Backquote
  | Letter Char
  | Delete | Insert
  | World Int
  | KeypadDigit Int
  | KeypadPeriod
  | KeypadDivide
  | KeypadMultiply
  | KeypadMinus
  | KeypadPlus
  | KeypadEnter
  | KeypadEquals
  | Up | Down | Right | Left
  | Home | End
  | PageUp | PageDown
  | Function Int
  | NumLock | CapsLock | ScrolLock
  | RShift | LShift
  | RCtrl | LCtrl
  | RAlt | LAlt
  | RCmdKey | LCmdKey -- Winkey/cmdkey
  | LSuper | RSuper
  | Mode
  | Compose
  | Help
  | PrintScreen
  | Sysreq
  | Break
  | Menu
  | Power
  | Euro
  | Undo
  deriving (Show, Read, Eq, Ord)

pretty :: Key -> String
pretty key = case key of
  Exclaim -> "!"
  DoubleQuote -> "\""
  Hash -> "#"
  Dollar -> "$"
  Ampersand -> "&"
  Quote -> "'"
  LeftParen -> "("
  RightParen -> ")"
  Asterisk -> "*"
  Plus -> "+"
  Comma -> ","
  Minus -> "-"
  Period -> "."
  Slash -> "/"
  (Digit n) -> show n
  Colon -> ":"
  Semicolon -> ";"
  Less -> "<"
  Equals -> "="
  Greater -> ">"
  QuestionMark -> "?"
  At -> "@"
  LeftBracket -> "["
  Backslash -> "\\"
  RightBracket -> "]"
  Caret -> "^"
  Underscore -> "_"
  Backquote -> "`"
  (Letter x) -> [x]
  KeypadDigit n -> "Keypad-" ++ show n
  KeypadPeriod -> "Keypad-Period"
  KeypadDivide -> "Keypad-Divide"
  KeypadMultiply -> "Keypad-Multiply"
  KeypadMinus -> "Keypad-Minus"
  KeypadPlus -> "Keypad-Plus"
  KeypadEnter -> "Keypad-Enter"
  KeypadEquals -> "Keypad-Equals"
  (Function n) -> 'F' : show n
  x -> show x
