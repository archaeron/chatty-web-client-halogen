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

foreign import data WEBSOCKET :: !
foreign import data WebSocket :: *

type WebSocketHandler eff = String -> Eff (eff) Unit

instance showWSResponse :: Show WSResponse where
	show Open = "Open"
	show Close = "Close"
	show (Message s) = "Message " ++ s

foreign import webSocketNative """
	function webSocketNative(url)
	{
		return function(handler)
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
								handler("opened the WS")();
								ws.onmessage = function(messageEvent)
								{
									handler(messageEvent.data)();
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
		};
	}
	""" :: forall e. WURL ->
				WebSocketHandler e ->
				(Error -> Eff e Unit) ->
				(WebSocket -> Eff e Unit) ->
				Eff e Unit

--mkWebsocket :: WebSocketConfig ->

webSocket :: forall e. WURL -> WebSocketHandler e -> Aff e WebSocket
webSocket url handlers = makeAff $ webSocketNative url handlers

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
