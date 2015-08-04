module Requests.WebSockets where

import Prelude
import Data.Function

import Control.Monad.Eff
import qualified Control.Monad.Eff.Exception (Error()) as E
import Control.Monad.Aff

type WURL = String

data WSResponse
	= Open
	| Close
	| Error String
	| Message String

foreign import data WS :: !
foreign import data WebSocket :: *

type WebSocketHandler eff = WSResponse -> Eff (ws :: WS | eff) Unit

instance showWSResponse :: Show WSResponse where
	show Open = "Open"
	show Close = "Close"
	show (Error e) = "Error " ++ e
	show (Message s) = "Message " ++ s

foreign import webSocketNative
	:: forall e. Fn2
			WURL
			(WebSocketHandler e)
			((E.Error -> Eff e Unit) -> (WebSocket -> Eff e Unit) -> Eff e Unit)

webSocket :: forall e. WURL -> WebSocketHandler e -> Aff e WebSocket
webSocket url handlers = makeAff $ runFn2 webSocketNative url handlers

foreign import push :: forall e. WebSocket -> String -> Eff e Unit
