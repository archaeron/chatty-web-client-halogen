module Models.Action where

import Models.Message

data Action
	= SendMessage MessageType
	| SetEditText String
	| SetChannels (Array Models.Channel.Channel)
	| SelectChannel Models.Channel.Channel
	| SelectInputType Models.Input.InputType
	| DoNothing
