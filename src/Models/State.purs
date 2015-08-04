module Models.State where

import Data.Maybe

import Models.Channel
import Models.Input
import Models.Message
import Models.User

type State =
	{ messages :: Array Message
	, editText :: String
	, user :: User
	, channels :: Array Channel
	, selectedChannel :: Maybe Channel
	--, selectedInputType :: InputType
	}

emptyState :: User -> State
emptyState user =
	{ messages: []
	, editText: ""
	, user: user
	, channels: []
	, selectedChannel: Nothing
	}
