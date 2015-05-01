module Views.Message
	( messagesView
	) where

import Data.Array
import Data.Maybe

import Control.Alternative

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as A
import qualified Halogen.HTML.Events.Handler as E

import qualified Halogen.Themes.Bootstrap3 as B

import Models.Channel
import Models.Message
import Models.State
import Views.Types

-- Classes
messageTextClass :: A.ClassName
messageTextClass = A.className "message-text"

messageCodeClass :: A.ClassName
messageCodeClass = A.className "message-code"


-- Views
messageTypeView :: forall p m. (Alternative m) => MessageType -> H.HTML p (m _)
messageTypeView message =
	case message of
		TextMessage { text: text } ->
			H.span [ A.class_ messageTextClass ] [ H.text text ]
		CodeMessage { language: language, text: text } ->
			H.pre [ A.class_ messageCodeClass ] [ H.text text ]
		otherwise ->
			H.span [ A.class_ $ A.className "message" ] []

messageView :: forall p m. (Alternative m) => Message -> H.HTML p (m _)
messageView { from: from, to: to, message: message } =
	H.div
		[ A.classes [ A.className "message", B.media ] ]
		[ H.div
			[ A.class_ B.mediaLeft ]
			[ H.img [ A.class_ B.mediaObject, A.src "images/avatar.svg" ] [ ]
			]
		, H.div
			[ A.class_ B.mediaBody ]
			[ H.h4 [ A.classes [ A.className "from", B.mediaHeading ] ] [ H.text from.name ]
			, H.span [ A.class_ $ A.className "message-content" ] [ messageTypeView message ]
			]
		]

messagesView :: forall p m. (Alternative m) => [Message] -> Maybe Channel -> H.HTML p (m _)
messagesView messages Nothing = H.div_ [ H.text "no channel selected" ]
messagesView messages (Just selectedChannel) =
	H.div
		[ A.classes [ A.className "messages" ] ]
		[ H.div
			[ A.class_ $ B.mediaBody ]
			[ H.h1 [ ] [ H.text (unChannel selectedChannel).name ]
			, H.div [ A.class_ $ A.className "messages-list" ] (messageView <$> messages)
			]
		]

--messagesComponent :: forall p m. (Applicative m) => Component p m in out
--messagesComponent
