module Views.Channel
 	( channelView
	, channelsView
	) where

import Data.Array

import Halogen

import Control.Alternative

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as A
import qualified Halogen.HTML.Events.Handler as E

import Models.Action
import Models.Channel
import Views.Types

import Models.State

channelViewSelectClass :: Channel -> Channel -> A.ClassName
channelViewSelectClass channel selectedChannel =
	if channel == selectedChannel then
		A.className "selected"
	else
		A.className "not-selected"

channelView :: forall p m. (Alternative m) => Channel -> Channel -> H.HTML p (m Input)
channelView selectedChannel channel =
	H.li [ A.class_ $ A.className "channel" ]
		[ H.span
			[ A.classes [ A.className "channel-name", channelViewSelectClass channel selectedChannel ]
			--, E.onClick ctx (const $ SelectChannel channel)
			]
			[ H.text (unChannel channel).name ]
		]

channelsView :: forall p m. (Alternative m) => [Channel] -> Channel -> H.HTML p (m Input)
channelsView channels selectedChannel =
	H.div [ A.class_ $ A.className "channels" ]
		[ H.h2 [] [ H.text "Channels" ]
		, H.ul [ A.class_ $ A.className "channels-list" ] ((channelView selectedChannel) <$> channels)
		]
