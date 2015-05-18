module Requests.Requests where

import Halogen (HalogenEffects())
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either
import Data.Foreign.Class
import Data.JSON
import Control.Monad.Aff
import Control.Alt
import Network.HTTP.Affjax
import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.Method (Method(..), methodToString)

import Requests.Types
import Models.Action
import Models.Message
import Models.Channel

apiUrl :: URL
apiUrl = "http://halogen.ctor.ch/v1/"

requestURL :: URL -> URL
requestURL url = apiUrl ++ url

postWithHeaders :: forall e a b. (Requestable a, Respondable b) => [RequestHeader] -> URL -> a -> Affjax e b
postWithHeaders headers url content =
	affjax $ defaultRequest
		{ method = POST
		, url = url
		, content = Just content
		, headers = headers
		}

getWithHeaders :: forall e a b. (Respondable b) => [RequestHeader] -> URL -> Affjax e b
getWithHeaders headers url =
	affjax $ defaultRequest
		{ method = GET
		, url = url
		, headers = headers
		}

headers :: [RequestHeader]
headers =
	[ RequestHeader "Authorization" "bla"
	, Accept Network.HTTP.MimeType.Common.applicationJSON
	, ContentType Network.HTTP.MimeType.Common.applicationJSON
	]

sampleMessage = MessageRequest
	{ content : "I'm a message"
	, title : "I'm a title"
	, lead : "I'm a lead"
	, contentType : "text"
	, to : 4
	, language : ""
	}

handler :: forall eff. String -> E.Event (HalogenEffects (ajax :: AJAX | eff)) Action
handler code = E.yield DoNothing `E.andThen` \_ -> E.async compileAff
	where
	compileAff :: Aff (HalogenEffects (ajax :: AJAX | eff)) Action
	compileAff = do
		result <- postWithHeaders headers (requestURL "messages") $ encode sampleMessage
		let response = decode result.response
		return case response of
			Just (MessageResponse msg) -> SendMessage $ TextMessage { text: msg.content }
			Nothing -> DoNothing

-- Channels

getChannelsRequest :: forall e a. (Respondable a) => Affjax e a
getChannelsRequest = getWithHeaders headers $ requestURL "channels"

getChannels :: forall eff. Aff (ajax :: AJAX | eff) [Channel]
getChannels = do
	result <- getChannelsRequest
	let response = decode result.response
	return $ fromMaybe [] response
