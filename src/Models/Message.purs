module Models.Message where

import Models.User

type TextMessage = { text :: String }

type CodeMessage = { language :: String, text :: String }

type FormulaMessage = { }

type FileMessage = { }

data MessageType
	= TextMessage TextMessage
	| CodeMessage CodeMessage
	| FormulaMessage FormulaMessage
	| FileMessage FileMessage

type Message = { from :: User, to :: User, message :: MessageType }
