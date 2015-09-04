module TimeTracking where

import Color exposing (..)
import Touch exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
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

-- MODEL

type alias Model =
  { events: List String }

initialModel : Model
initialModel =
  { events = [] }

-- UPDATE

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

-- VIEW

view : Address Action -> Model -> Html
view address model =
  div [ class "wrapper" ]
  [
    div [ class "main_item"]
    [
      p []
        [ text "Wähle eine Sprache" ],
      a [ class "big_button", href "#"]
        [
          p []
          [ text "Deutsch" ]
        ]
    ],
    div [ class "main_item"]
    [
      p []
        [ text "Select your language" ],
      a [ class "big_button", href "#"]
        [
          p []
          [ text "English" ]
        ]
    ]
  ]
  -- div [ class "wrapper" ]
  -- [
  --   div [ class "main_item"]
  --   [
  --     p []
  --       [ text "Wähle eine Sprache" ]
  --   ],
  --   div [ class "main_item"]
  --   [
  --     p []
  --       [ text "Select your language" ]
  --   ]
  -- ]
