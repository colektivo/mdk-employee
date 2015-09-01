module TimeTracking where

import Color exposing (..)
import Touch exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import String
import Window
import Time exposing (..)
import Signal exposing (..)
import StartApp.Simple as StartApp

actions: Mailbox Action
actions =
  mailbox Reset


main : Signal Html
main =
  StartApp.start
    { model = initialModel,
      view = view,
      update = update}

type alias Model =
  { events: List String }

initialModel : Model
initialModel =
  { events = [] }

view : Address Action -> Model -> Html
view address model =
  div []
    [ button [ onClick address (Mark "X") ]
        [ text "Mark" ],
      button [ onClick address Reset ]
        [ text "Reset" ],
      h2 []
        [ text (model.events |> List.length |> toString), text " Events" ],
      div []
        (List.map (\t -> text t) model.events)]

type Action = Mark String
            | Reset



addName : String -> a -> { a | name:String }
addName name record = { record | name = name }

update : Action -> Model -> Model
update action log =
  case action of
    Mark date ->
      { log | events <- log.events ++ [date] }
    Reset ->
      { log | events <- [] }
