module Models.State where

import Data.Maybe

import Models.Channel
import Models.Input
import Models.Message
import Models.User

type State =
    { messages :: [ Message ]
    , editText :: String
    , user :: User
    , channels :: [ Channel ]
    , selectedChannel :: Channel
    , selectedInputType :: InputType
    }
