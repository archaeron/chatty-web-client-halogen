module Requests.Requests where

import Halogen (HalogenEffects())
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E

import Data.Either
import Data.Foreign.Class
import Control.Monad.Aff
import Control.Alt
import Network.HTTP.Affjax

import Models.Action
import Models.Message

handler :: forall eff. String -> E.Event (HalogenEffects (ajax :: AJAX | eff)) Action
handler code = E.yield DoNothing `E.andThen` \_ -> E.async compileAff
	where
	compileAff :: Aff (HalogenEffects (ajax :: AJAX | eff)) Action
	compileAff = do
		result <- post "http://try.purescript.org/compile/text" code
		let response = result.response
		return case readProp "js" response <|> readProp "error" response of
			Right js -> SendMessage $ TextMessage { text: js }
			Left _ -> DoNothing
