module Main where

import Prelude

import Data.Array (range)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray, fromCharArray)
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Action =
    Clear
  | Calculate
  | Insert String

type State = Array Char


component :: ∀ query input output m. H.Component HH.HTML query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: ∀ input. input -> State
initialState _ = []

render :: ∀ m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "container" ]]
    $ [ HH.br_
    , HH.input [ HP.type_  HP.InputText, HP.readOnly true, HP.value $ fromCharArray state ]
    ] <> funcpad <> numberpad <> operpad

numberpad :: ∀ m. Array(H.ComponentHTML Action () m)
numberpad = do
  x <- range 0 9
  pure $ HH.button
    [ HE.onClick \_ -> Just $ Insert $ show x
    , HP.classes [HH.ClassName "btn", HH.ClassName "btn-danger" ]]
    [ HH.text $ show x ]

operpad :: ∀ m. Array(H.ComponentHTML Action () m)
operpad = do
  x <- ["+","-","*","/"]
  pure $ HH.button
    [ HE.onClick \_ -> Just $ Insert x
    , HP.classes [HH.ClassName "btn", HH.ClassName "btn-primary" ]]
    [ HH.text x ]

funcpad :: ∀ m. Array(H.ComponentHTML Action () m)
funcpad = do
  x <- ["C","="]
  pure $ HH.button
    [ HE.onClick \_ -> Just Clear
    , HP.classes [HH.ClassName "btn", HH.ClassName "btn-success" ]]
    [ HH.text x ]

handleAction :: ∀ output m. Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Clear -> H.put []
  Calculate -> H.modify_ \state -> state
  Insert s -> H.modify_ \state -> state <> (toCharArray s)