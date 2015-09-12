module TimeTracking where

import Graphics.Element exposing (show)
import Task             exposing (Task, andThen)
import String
import Time
import SocketIO
import Html             exposing (..)
import Html.Attributes  exposing (..)
import Html.Events      exposing (onClick)
import Signal           exposing (..)

socket : Task x SocketIO.Socket
socket = SocketIO.io "http://localhost:8001" SocketIO.defaultOptions

port initial : Task x ()
port initial = socket `andThen` SocketIO.emit "" "Hello I am a browser using websockets"

port responses : Task x ()
port responses = socket `andThen` SocketIO.on "completed" received.address


-- MODEL

type alias Model =
  {  events: List String,
    page: Int,
    language: String
  }

initialModel : Model
initialModel =
  {
    events = [],
    page = 0,
    language = ""
  }


-- ACTIONS

type Action = Next
          | Previous
          | Reset
          | Start String
          | Language String

-- UPDATE

update : Action -> Model -> Model
update action log =
  case action of
    Previous ->
      { log | page <- log.page - 1 }
    Next ->
      { log | page <- log.page + 1 }
    Start data ->
      { log | events <- log.events ++ [data] }
    Reset ->
      { log | events <- [], page <- 0 }
    Language language ->
      { log | language <- language, page <- 2}

-- SIGNALS

actions: Mailbox Action
actions =
  mailbox Reset

received : Signal.Mailbox String
received =
  Signal.mailbox ""

cardinput : Signal Action
cardinput =
  Signal.map Start received.signal

allinputs : Signal Action
allinputs = Signal.merge actions.signal cardinput

model: Signal Model
model =
  foldp update initialModel allinputs


-- VIEW

previous: Address Action -> Model -> Html
previous address model =
  li [ onClick address Previous ] [ text "Prev" ]

next: Address Action -> Model -> Html
next address model =
  li [ onClick address Next ] [ text "Next" ]

-- navigation: Address Action -> Model -> Html
-- navigation address model =
--   let
--     links =
--       if model.page > 0 then [previous address model] else []
--     alllinks =
--       if model.page < 5 then links ++ [next address model] else links
--   in
--     ul []
--       alllinks

phrase: String -> String -> Html
phrase key language =
  case language of
    "de" ->
      case key of
        "welcome" -> text "Willkommen!"
        "place_chip" -> text "Bitte halte deine Chipkarte auf das Lesegerät um deine persönliche Auswertung zu erhalten"
    "en" ->
      case key of
        "welcome" -> text "Welcome!"
        "place_chip" -> text "Please place your chipcard on the reading device to receive your personal evaluation"

content: Address Action -> Model -> Html
content address model =
  case model.page of
    0 ->
      div [ class "wrapper" ]
      [
        div [ class "main_item"]
        [
          p []
          [ phrase "place_chip" "de" ]
        ],
        div [ class "main_item"]
        [
          p []
          [ phrase "place_chip" "en" ]
        ],
        a [ onClick address Next]
        [
          text "Next"
        ]
      ]
    1 ->
      div [ class "wrapper -second" ]
      [
        div [ class "main_item"]
        [
          p []
          [ phrase "welcome" "de" ],
          a [ onClick address (Language "de"), class "big_button", href "#"]
          [
            p []
            [ text "Deutsch" ]
          ]
        ],
        div [ class "main_item"]
        [
          p []
          [ phrase "welcome" "en" ],
          a [ onClick address (Language "en"), class "big_button", href "#"]
          [
            p []
            [ text "English" ]
          ]
        ]
      ]
    2 ->
      div [ class "wrapper -second" ]
      [
        div [ class "main_item"]
        [
          p []
          [ phrase "welcome" model.language ]
        ]
      ]

view : Address Action -> Model -> Html
view address model =
  div []
    [
      -- navigation address model,
      content address model
      -- ,
      -- h1 [ onClick address Reset ]
      --   [ text "Reset" ],
      -- div []
      --   (List.map (\t -> text t) model.events),
      -- div []
      --   [ text (toString model.page) ]
    ]


-- MAIN

main : Signal Html
main =
  map (view actions.address) model
