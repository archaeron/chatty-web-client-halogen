module Requests.Types where

import Data.JSON

data MessageRequest = MessageRequest
	{ content :: String
	, title :: String
	, lead :: String
	, contentType :: String
	, to :: Number
	, language :: String
	}

instance barToJSON :: ToJSON MessageRequest where
	toJSON (MessageRequest msg) =
		object
			[ "content" .= msg.content
			, "title" .= msg.title
			, "lead" .= msg.lead
			, "contentType" .= msg.contentType
			, "to" .= msg.to
			, "language" .= msg.language
			]


data MessageResponse = MessageResponse
	{ content :: String
	, contentType :: String
	, created :: String
	, from :: Number
	, language :: String
	, lead :: String
	, title :: String
	, to :: Number
	}

instance barFromJSON :: FromJSON MessageResponse where
	parseJSON (JObject o) = do
		content <- o .: "content"
		contentType <- o .: "contentType"
		created <- o .: "created"
		from <- o .:  "from"
		language <- o .:  "language"
		lead <- o .:  "lead"
		title <- o .:  "title"
		to <- o .:  "to"
		return $ MessageResponse
			{ content: content
			, contentType: contentType
			, created: created
			, from: from
			, language: language
			, lead: lead
			, title: title
			, to: to
			}
	parseJSON _ = fail "Bar parse failed."
