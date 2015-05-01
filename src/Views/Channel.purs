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

channelViewSelectClass :: Channel -> Channel -> A.ClassName
channelViewSelectClass channel selectedChannel =
    if channel == selectedChannel then
        A.className "selected"
    else
        A.className "not-selected"

channelView :: forall p m. (Alternative m) => Channel -> Channel -> H.HTML p (m AS.Input)
channelView selectedChannel channel =
    H.li [ A.class_ $ A.className "channel" ]
        [ H.span
            [ A.classes [ A.className "channel-name", channelViewSelectClass channel selectedChannel ]
            --, E.onClick ctx (const $ SelectChannel channel)
            ]
            [ H.text (unChannel channel).name ]
        ]

channelsView :: forall p m. (Alternative m) => [Channel] -> Channel -> H.HTML p (m AS.Input)
channelsView channels selectedChannel =
    H.div [ A.class_ $ A.className "channels" ]
        [ H.h2_ [ H.text "Channels" ]
        , H.ul [ A.class_ $ A.className "channels-list" ] ((channelView selectedChannel) <$> channels)
        ]

channelsComponent :: forall p m. (Applicative m) => Component p m Channel [Channel]
channelsComponent = component (render <$> stateful (State Nothing) update)
    where
    render :: State -> H.HTML _ (m [Channel])
    render (State channel) =
        H.div_
            [ H.span [ A.onClick $ A.input_ [(Channel { name: "Hello" })] ] [ H.text "Channel" ]
            , H.span_ [ H.text $ showChannel channel ]
            ]

    update :: State -> Channel -> State
    update (State _) channel = State (Just channel)


showChannel :: Maybe Channel -> String
showChannel maybeChannel =
    maybe "" (\(Channel c) -> c.name) maybeChannel
