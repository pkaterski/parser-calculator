module Main where

import Prelude

import Data.Array (range, (:), head, tail)
import Data.Int (radix, toStringAs)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource (eventListenerEventSource)
import Halogen.VDom.Driver (runUI)
import Parser (expr, runParser)
--import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Action
  = Initialize
  | HandleKey H.SubscriptionId KE.KeyboardEvent
  | Clear
  | Calculate
  | Insert String
  | Undo

data State =
    Working { current :: (Array Char), previous :: Array (Array Char) }
  | Error { message :: String, stateHistory :: Array (Array Char) }

derive instance eqState :: Eq State

instance showState :: Show State where
  show (Working { current: arr }) = fromCharArray arr
  show (Error { message: err }) = err

stateToError :: State -> String -> State
stateToError (Working { current: now, previous: old }) m =
  Error { message: m,  stateHistory: now : old }
stateToError (Error { message: _, stateHistory: h }) m =
  Error { message: m, stateHistory: h }


component :: ∀ query input output m. MonadAff m => H.Component HH.HTML query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
    }

initialState :: ∀ input. input -> State
initialState _ = Working { current: ['0'], previous: [] }

render :: ∀ m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "container" ]]
    $ [ HH.br_
    , HH.input
      [ HP.id_ "screen"
      , HP.type_  HP.InputText
      , HP.readOnly true
      , HP.value $ show state
      ]
    , HH.br_]
    <> funcpad
    <> numberpad
    <> bracketpad
    <> operpad

numberpad :: ∀ m. Array(H.ComponentHTML Action () m)
numberpad = do
  x <- range 0 9
  pure $ HH.button
    [ HE.onClick \_ -> Just $ Insert $ show x
    , HP.classes [HH.ClassName "btn", HH.ClassName "btn-danger" ]]
    [ HH.text $ show x ]

operpad :: ∀ m. Array(H.ComponentHTML Action () m)
operpad = do
  x <- ["+","-","*"]
  pure $ HH.button
    [ HE.onClick \_ -> Just $ Insert x
    , HP.classes [HH.ClassName "btn", HH.ClassName "btn-primary" ]]
    [ HH.text x ]

bracketpad :: ∀ m. Array(H.ComponentHTML Action () m)
bracketpad = do
  x <- ["(",")"]
  pure $ HH.button
    [ HE.onClick \_ -> Just $ Insert x
    , HP.classes [HH.ClassName "btn", HH.ClassName "btn-warning" ]]
    [ HH.text x ]

funcpad :: ∀ m. Array(H.ComponentHTML Action () m)
funcpad =
  [  HH.button
      [ HE.onClick \_ -> Just Clear
      , HP.classes [HH.ClassName "btn", HH.ClassName "btn-success" ]]
      [ HH.text "C" ]
  , HH.button
      [ HE.onClick \_ -> Just Calculate
      , HP.classes [HH.ClassName "btn", HH.ClassName "btn-success" ]]
      [ HH.text "=" ]
  , HH.button
      [ HE.onClick \_ -> Just Undo
      , HP.classes [HH.ClassName "btn", HH.ClassName "btn-success" ]]
      [ HH.text "undo" ]
  ]

handleAction :: ∀ output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Clear -> H.put $ initialState unit
  Calculate -> H.modify_ calculate
  Insert s -> H.modify_ $ insertString s
  Undo -> H.modify_ undo
  Initialize -> do
    document <- H.liftEffect $ document =<< window
    H.subscribe' \sid ->
      eventListenerEventSource
        KET.keydown
        (HTMLDocument.toEventTarget document)
        (map (HandleKey sid) <<< KE.fromEvent)

  HandleKey sid ev -> do
    let char = KE.key ev
    when (String.length char == 1) do
      handleAction $ Insert char

undo :: State -> State
undo s@(Error { stateHistory: h }) = try restore h s
undo s@(Working { current: now, previous: old }) = try restore old s

try :: ∀ a b. (b -> Maybe a) -> b -> a -> a
try f i unchanged = case f i of
  Just v -> v
  Nothing -> unchanged

restore :: Array (Array Char) -> Maybe State
restore arr = do
  x  <- head arr
  xs <- tail arr
  pure $ Working { current: x, previous: xs }


insertString :: String -> State -> State
insertString s (Working { current: state, previous: old }) =
  if state == ['0']
  then Working { current: toCharArray s, previous: state : old }
  else Working { current: state <> toCharArray s, previous: state : old }
insertString _ (Error err) = Error err

insertIfDifferent :: ∀ a. Eq a => a -> a -> Array a -> Array a
insertIfDifferent a toCheck arr =
  if a == toCheck
  then arr
  else a : arr

calculate :: State -> State
calculate w@(Working { current: s, previous: old }) =
  case runParser expr s of
    Just (Tuple [] n) ->
      case radix 10 of -- this shit is necessary..
        Just r ->
          let new = toCharArray $ toStringAs r n
          in Working { current: new, previous: insertIfDifferent s new old }
        Nothing -> stateToError w "the impossible has happened" -- never gonna go here
    Just (Tuple remainder _) -> stateToError w $ "unparsable after: " <> fromCharArray remainder
    Nothing -> stateToError w "unparsable"
calculate (Error err) = Error err
