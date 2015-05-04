module Views.Channel
     ( channelView
    , channelsView
    ) where

--import Data.Array
import Data.Maybe

import Halogen

import Control.Alternative

import Halogen.Component
import Halogen.Signal
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as A
import qualified Halogen.HTML.Events.Handler as E

import Models.Action
import Models.Channel
import Views.Types

import qualified Models.State as AS

data State = State (Maybe Channel)

-- Classes

selectedClass :: A.ClassName
selectedClass = A.className "selected"

notSelectedClass :: A.ClassName
notSelectedClass = A.className "not-selected"

--- Views

channelViewSelectClass :: Maybe Channel -> Channel -> A.ClassName
channelViewSelectClass Nothing channel = notSelectedClass
channelViewSelectClass (Just selectedChannel) channel =
    if channel == selectedChannel then
        selectedClass
    else
        notSelectedClass

channelView :: forall m. (Alternative m) => Maybe Channel -> Channel -> H.HTML (m _)
channelView selectedChannel channel =
    H.li [ A.class_ $ A.className "channel" ]
        [ H.span
            [ A.classes [ A.className "channel-name", channelViewSelectClass selectedChannel channel ]
            --, E.onClick ctx (const $ SelectChannel channel)
            ]
            [ H.text (unChannel channel).name ]
        ]

channelsView :: forall m. (Alternative m) => [Channel] -> Maybe Channel -> H.HTML (m _)
channelsView channels selectedChannel =
    H.div [ A.class_ $ A.className "channels" ]
        [ H.h2_ [ H.text "Channels" ]
        , H.ul [ A.class_ $ A.className "channels-list" ] ((channelView selectedChannel) <$> channels)
        ]
