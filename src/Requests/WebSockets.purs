module Requests.WebSockets where

import Data.Function

import Control.Monad.Eff
import Control.Monad.Eff.Exception (Error())
import Control.Monad.Aff

type WURL = String

data WSResponse
	= Open
	| Close
	| Message String

foreign import data WS :: !
foreign import data WebSocket :: *

type WebSocketHandler eff = WSResponse -> Eff (ws :: WS | eff) Unit

instance showWSResponse :: Show WSResponse where
	show Open = "Open"
	show Close = "Close"
	show (Message s) = "Message " ++ s

foreign import webSocketNative """
	function webSocketNative(url, handler)
	{
		return function(err)
		{
			return function(callback)
			{
				return function()
				{
					try
					{
						var ws = new WebSocket(url);
						ws.onopen = function()
						{
							handler(new Open)();
							ws.onmessage = function(messageEvent)
							{
								handler(new Message(messageEvent.data))();
							};
							callback(ws)();
						};

					}
					catch (e)
					{
						err(e)();
					}
				};
			};
		};
	}
	""" :: forall e. Fn2
						WURL
						(WebSocketHandler e)
						((Error -> Eff e Unit) -> (WebSocket -> Eff e Unit) -> Eff e Unit)

webSocket :: forall e. WURL -> WebSocketHandler e -> Aff e WebSocket
webSocket url handlers = makeAff $ runFn2 webSocketNative url handlers

foreign import push """
	function push(ws)
	{
		return function(msg)
		{
			return function()
			{
				ws.send(msg);
				return {};
			};
		};
	}
	""" :: forall e. WebSocket -> String -> Eff e Unit
