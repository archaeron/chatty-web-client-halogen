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
import Views.Input
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
			{ text: """Two households, both alike in dignity,
  In fair Verona, where we lay our scene,
From ancient grudge break to new mutiny,
  Where civil blood makes civil hands unclean.
From forth the fatal loins of these two foes
  A pair of star-cross'd lovers take their life;
Whose misadventur'd piteous overthrows
  Doth with their death bury their parents' strife.
The fearful passage of their death-mark'd love,
  And the continuance of their parents' rage,
Which but their children's end naught could remove,
  Is now the two hours' traffic of our stage;
The which, if you with patient ears attend,
What here shall miss, our toil shall strive to mend."""
			}
		}
	,
		{ from: user2
		, to: user1
		, message: TextMessage
			{ text: """
			Actual happiness always looks pretty squalid in comparison with the overcompensations for misery.
			And, of course, stability isn't nearly so spectacular as instability.
			And being contented has none of the glamour of a good fight against misfortune, none of the picturesqueness of a struggle with temptation, or a fatal overthrow by passion or doubt. Happiness is never grand.
			- Aldous Huxley, Brave New World"""
			}
		}
	,
		{ from: user1
		, to: user2
		, message: TextMessage
			{ text: """Most human beings have an almost infinite capacity for taking things for granted. - Aldous Huxley, Brave New World"""
			}
		}
	,
		{ from: user2
		, to: user1
		, message: CodeMessage
			{ language: "haskell"
			, text: "add a b = a + b" }
		}
	,
		{ from: user1
		, to: user2
		, message: CodeMessage
			{ language: "javascript"
			, text: "function(a, b) { return a + b; }" }
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

testState :: State
testState =
	{ messages: messages
	, user: user1
	, channels: channels
	, selectedChannel: Just (Channel { name: "PureScript" })
	}

-- | The view is a state machine, consuming inputs, and generating HTML documents which in turn, generate new inputs
ui :: forall p m. (Alternative m) => Component m _ _
ui = render <$> stateful testState update
	where
	render :: State -> H.HTML (m _)
	render st =
		H.div
			[ A.class_ B.container ]
			[ H.div
				[ A.class_ B.row ]
				[ H.div
					[ A.class_ B.colMd2 ]
					[ channelsView channels st.selectedChannel
					]
				, H.div
					[ A.class_ B.colMd10 ]
					[ messagesView st.messages st.selectedChannel
					]
				]
			, H.div
				[ A.class_ B.row ]
				[ H.div
					[ A.classes [ B.colMd10, B.colMdOffset2 ] ]
					[ inputView
					]
				]
			]

	update :: State -> Action -> State
	update st (SendMessage message) =
		st { messages = st.messages ++ [{ from: st.user, to: user2, message: message }] }
	update st (SelectChannel channel) =
		st { selectedChannel = Just channel }
	update st _ = st

main = do
	Tuple node driver <- runUI ui
	appendToBody node
