module Models.State where

import Data.Maybe

import Models.Channel
import Models.Input
import Models.Message
import Models.User

type State =
    { messages :: [ Message ]
    --, editText :: String
    , user :: Maybe User
    , channels :: [ Channel ]
    , selectedChannel :: Maybe Channel
    --, selectedInputType :: InputType
    }

emptyState :: State
emptyState =
	{ messages: []
	, user: Nothing
	, channels: []
	, selectedChannel: Nothing
	}
