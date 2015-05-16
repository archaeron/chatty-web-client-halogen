module Requests.Requests where

import Halogen (HalogenEffects())
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E

import Data.Maybe (Maybe(..), maybe)
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

postWithHeaders :: forall e a b. (Requestable a, Respondable b) => [RequestHeader] -> URL -> a -> Affjax e b
postWithHeaders headers url content =
	affjax $ defaultRequest
		{ method = POST
		, url = url
		, content = Just content
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
		result <- postWithHeaders headers "http://127.0.0.1:8083/messages" $ encode sampleMessage
		let response = decode result.response
		return case response of
			Just (MessageResponse msg) -> SendMessage $ TextMessage { text: msg.content }
			Nothing -> DoNothing

--getMessages :: forall eff. E.Event (HalogenEffects (ajax :: AJAX | eff)) Action
--getMessages = do
--	result <- get "/api"
--	return DoNothing
