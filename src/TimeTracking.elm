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


import Graphics.Element exposing (show)
import Task exposing (Task, andThen)
import SocketIO

socket : Task x SocketIO.Socket
socket = SocketIO.io "http://localhost:8001" SocketIO.defaultOptions

port initial : Task x ()
port initial = socket `andThen` SocketIO.emit "" "Hello I am a browser using websockets"


port responses : Task x ()
port responses = socket `andThen` SocketIO.on "completed" received.address

-- Model

type alias Model =
  { events: List String,
    page: Int }

initialModel : Model
initialModel =
  {
    events = [],
    page = 0
    }

-- actions

type Action = Next
            | Previous
            | Reset
            | Start String

-- update

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
      { log | events <- [] , page <- 0 }

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


-- view

previous: Address Action -> Model -> Html
previous address model =
  li [ onClick address Previous ] [ text "Prev" ]

next: Address Action -> Model -> Html
next address model =
  li [ onClick address Next ] [ text "Next" ]

navigation: Address Action -> Model -> Html
navigation address model =
  let
    links =
      if model.page > 0 then [previous address model] else []
    alllinks =
      if model.page < 5 then links ++ [next address model] else links
  in
    ul []
      alllinks

view : Address Action -> Model -> Html
view address model =
  div []
    [ navigation address model,
      h1 [ onClick address Reset ]
        [ text "Reset" ],
      div []
        (List.map (\t -> text t) model.events),
      div []
        [ text (toString model.page) ]
      ]


-- main

main : Signal Html
main =
  map (view actions.address) model
