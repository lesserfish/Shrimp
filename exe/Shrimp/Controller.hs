module Shrimp.Controller where

data Key = A | B | UP | Down | Left | Right | Select | Start
type KeyState = Bool

data Controller = Controller
    { cA :: KeyState
    , cB :: KeyState
    , cUp :: KeyState
    , cDown :: KeyState
    , cLeft :: KeyState
    , cRight :: KeyState
    , cSelect :: KeyState
    , cStart :: KeyState
    }

setKey :: Key -> KeyState -> Controller -> Controller
setKey = undefined

getKey :: Key -> Controller -> KeyState
getKey = undefined
