module Views.Input
    ( inputView
    ) where

import Control.Alternative

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as A
import qualified Halogen.HTML.Events.Handler as E


inputView :: forall m. (Alternative m) => H.HTML (m _)
inputView =
    H.div [ A.class_ $ A.className "input-view" ] [ H.text "input view" ]
