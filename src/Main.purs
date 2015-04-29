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
import Views.Message

appendToBody :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToBody e = document globalWindow >>= (body >=> flip appendChild e)

newtype Task = Task { description :: String, completed :: Boolean }

-- | The state of the application
data State = State [Task]

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

instance inputSupportsUndoRedo :: Undo.SupportsUndoRedo Input where
    fromUndoRedo Undo.Undo = Undo
    fromUndoRedo Undo.Redo = Redo
    toUndoRedo Undo = Just Undo.Undo
    toUndoRedo Redo = Just Undo.Redo
    toUndoRedo _ = Nothing

-- | The view is a state machine, consuming inputs, and generating HTML documents which in turn, generate new inputs
ui :: forall p m. (Alternative m) => Component p m Input Input
ui = component (render <$> stateful (Undo.undoRedoState (State [])) (Undo.withUndoRedo update))
    where
    render :: forall p. Undo.UndoRedoState State -> H.HTML p (m Input)
    render st =
        case Undo.getState st of
            State ts ->
                H.div
                    [ A.class_ B.container ]
                    [ channelsView [ Channel { name: "Hi" } ] (Channel { name: "Hi" })
                    , messagesView messages (Channel { name: "Hi" })
                    ]

    update :: State -> Input -> State
    update (State ts) (NewTask s) = State (ts ++ [Task { description: fromMaybe "" s, completed: false }])
    update (State ts) (UpdateDescription i description) = State $ modifyAt i (\(Task t) -> Task (t { description = description })) ts
    update (State ts) (MarkCompleted i completed) = State $ modifyAt i (\(Task t) -> Task (t { completed = completed })) ts
    update (State ts) (RemoveTask i) = State $ deleteAt i 1 ts

main = do
    Tuple node driver <- runUI ui
    appendToBody node
    Router.onHashChange (NewTask <<< Just <<< S.drop 1 <<< Router.runHash) driver
