// module Requests.WebSockets

exports.webSocketNative = function(url, handler)
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
						ws.onerror = function(e)
						{
							handler(new Error(e))();
						}
						ws.onclose = function()
						{
							handler(new Close)();
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
