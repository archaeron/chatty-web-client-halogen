// module Requests.WebSockets

exports.webSocketNative = function(open, close, error, message, url, handler)
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
						handler(open)();
						ws.onmessage = function(messageEvent)
						{
							handler(message(messageEvent.data))();
						};
						ws.onerror = function(e)
						{
							handler(error(e))();
						}
						ws.onclose = function()
						{
							handler(close)();
						}
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

exports.push = 	function(ws)
{
	return function(msg)
	{
		return function()
		{
			ws.send(msg);
			return {};
		};
	};
};
