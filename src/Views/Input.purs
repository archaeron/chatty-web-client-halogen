module Views.Input
	( inputView
	) where

import Control.Alternative
import Data.Maybe

import Halogen
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as A
import qualified Halogen.HTML.Events.Monad as E

import qualified Halogen.Themes.Bootstrap3 as B

import Models.Action
import Models.Message
import Models.State

import Requests.Requests (postMessage)

import Control.Monad.Eff
import Control.Monad.Eff.Class

import Network.HTTP.Affjax

inputView :: forall eff. State -> H.HTML (E.Event (HalogenEffects (ajax :: AJAX | eff)) Action)
inputView st =
	case st.selectedChannel of
		Just channel ->
			H.div
				[ A.class_ $ A.className "input-view" ]
				[ H.textarea
					[ A.classes [ B.formControl ]
					, A.placeholder "Message"
					, A.onInput (A.input SetEditText)
					]
					[ H.text st.editText ]
				, H.button
					[ A.onClick  (\_ -> pure $ postMessage channel st.editText)
					]
					[ H.text "Send"
					]
				]
		Nothing -> H.div_ []
