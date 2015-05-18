module Models.Message where

import Models.Channel
import Models.User

type TextMessage = { text :: String }

type CodeMessage = { language :: String, text :: String }

type FormulaMessage = { text :: String }

type FileMessage = { }

data MessageType
	= TextMessage TextMessage
	| CodeMessage CodeMessage
	| FormulaMessage FormulaMessage
	| FileMessage FileMessage

type Message =
	{ from :: User
	, to :: Channel
	, message :: MessageType
	}
