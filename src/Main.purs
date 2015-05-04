module Main where

import Data.Void
import Data.Tuple
import Data.Maybe
import Data.Either
import Data.Array (zipWith, length, modifyAt, deleteAt, (..), (!!), map)

import qualified Data.String as S

import Debug.Trace

import Control.Functor (($>))
import Control.Alternative
import Control.Bind
import Control.Monad.Eff

import DOM

import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window

import Halogen
import Halogen.Signal
import Halogen.Component

import qualified Halogen.Mixin.UndoRedo as Undo
import qualified Halogen.Mixin.Router as Router

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as A
import qualified Halogen.HTML.Events.Handler as E

import qualified Halogen.Themes.Bootstrap3 as B
import qualified Halogen.Themes.Bootstrap3.InputGroup as BI

import Models.Action
import Models.Channel
import Models.Group
import Models.Input
import Models.User
import Models.Message
import Models.State
import Views.Channel
--import Views.Input
import Views.Message

appendToBody :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToBody e = document globalWindow >>= (body >=> flip appendChild e)

user1 :: User
user1 =
    { name: "Harry"
    , email: "harry@hogwarts.com"
    }

user2 :: User
user2 =
    { name: "Ben"
    , email: "ben@kenobi.ch"
    }

messages :: [Message]
messages =
    [
        { from: user1
        , to: user2
        , message: TextMessage
            { text: "Hello"
            }
        }
    ,
        { from: user1
        , to: user2
        , message: TextMessage
            { text: "Hello"
            }
        }
    ]

channels :: [ Channel ]
channels =
    [ Channel { name: "PureScript" }
    , Channel { name: "F#" }
    , Channel { name: "Haskell" }
    , Channel { name: "Elm" }
    , Channel { name: "Idris" }
    ]

-- | The view is a state machine, consuming inputs, and generating HTML documents which in turn, generate new inputs
ui :: forall p m. (Alternative m) => Component m _ _
ui = render <$> stateful emptyState update
    where
    render :: State -> H.HTML (m _)
    render st =
        H.div
            [ A.class_ B.container ]
            [ H.div
                [ A.class_ B.row ]
                [ H.div
                    [ A.class_ B.colMd3 ]
                    [ channelsView channels st.selectedChannel
                    ]
                , H.div
                    [ A.class_ B.colMd9 ]
                    [ messagesView messages st.selectedChannel
                    ]
                ]
            ]

    update :: State -> _ -> State
    update st _ = st

main = do
    Tuple node driver <- runUI ui
    appendToBody node
