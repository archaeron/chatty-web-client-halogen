module Views.Message where

import Data.Array

import Control.Alternative

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as A
import qualified Halogen.HTML.Events.Handler as E

import Models.Channel
import Models.Message
import Models.State
import Views.Types

messageTypeView :: forall p m. (Alternative m) => MessageType -> H.HTML p (m Input)
messageTypeView message =
	case message of
		TextMessage { text: text } ->
			H.span [ A.class_ $ A.className "message-text" ] [ H.text text ]
		CodeMessage { language: language, text: text } ->
			H.pre [ A.class_ $ A.className "message-code" ] [ H.text text ]
		otherwise ->
			H.span [ A.class_ $ A.className "message" ] []

messageView :: forall p m. (Alternative m) => Message -> H.HTML p (m Input)
messageView { from: from, to: to, message: message } =
	H.li
		[ A.class_ $ A.className "message" ]
		[ H.span [ A.class_ $ A.className "from" ] [ H.text from.name ]
		, H.span [ A.class_ $ A.className "message-content" ] [ messageTypeView message ]
		]

messagesView :: forall p m. (Alternative m) => [Message] -> Channel -> H.HTML p (m Input)
messagesView messages selectedChannel =
	H.div
		[ A.class_ $ A.className "messages" ]
		[ H.h2 [] [ H.text (unChannel selectedChannel).name ]
		, H.ul [ A.class_ $ A.className "messages-list" ] (messageView <$> messages)
		]
