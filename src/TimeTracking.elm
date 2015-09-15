module TimeTracking where

import Graphics.Element exposing (show)
import Task             exposing (Task, andThen)
import String
import Keyboard
import Time             exposing (..)
import SocketIO
import Html             exposing (..)
import Html.Attributes  exposing (..)
import Html.Events      exposing (onClick)
import Signal           exposing (..)
import VisitorData      exposing (..)
import Translations     exposing (..)

socket : Task x SocketIO.Socket
socket = SocketIO.io "http://localhost:8001" SocketIO.defaultOptions

-- configuration port the socket server will send a message to start with the current configuration
port initial : Task x ()
port initial = socket `andThen` SocketIO.on "init" init.address

-- messages port where we will receive the visitorData
port responses : Task x ()
port responses = socket `andThen` SocketIO.on "completed" received.address

-- MODEL

type alias Model =
  {  visitorData: VisitorData,
    page: Int,
    timeoutOnPage: Int,
    timeoutInSeconds: Int,
    seconds: Int,
    language: String
  }

initialModel : Model
initialModel =
  {
    visitorData = defaultCardData,
    timeoutInSeconds = 5,
    timeoutOnPage = 7,
-- page -1 server not initiated
    page = -1,
    seconds = 0,
    language = ""
  }

-- ACTIONS

type Action =   Next
          | Init String
          | Previous
          | Reset
          | Start String
          | Language String
          | Subpage Int
          | LeaveSubpage
          | NoOp
          | UpdateClock Float

-- UPDATE

update : Action -> Model -> Model
update action log =
  case action of
    Init config ->
      update Next log
    NoOp ->
      log
    UpdateClock _ ->
      let timeout = log.page == log.timeoutOnPage && log.seconds > log.timeoutInSeconds
      in
        if timeout then update Reset log
           else { log | seconds <- log.seconds + 1 }
    Previous ->
      { log | page <- log.page - 1, seconds <- 0 }
    Next ->
      { log | page <- log.page + 1, seconds <- 0 }
    Start data ->
      { log | visitorData <- toValidVisitorData ( decodeVisitorData data ) , page <- 1 }
    Reset ->
      { log | visitorData <- defaultCardData , page <- 0, seconds <- 0 }
    Language language ->
      { log | language <- language, page <- 2}
    Subpage subpage ->
      { log | page <- 600 + subpage}
    LeaveSubpage ->
      { log | page <- 6}


-- SIGNALS

direction: Signal Int
direction =
  Signal.map .x Keyboard.arrows

directionToAction: Int -> Action
directionToAction direction =
  case direction + 1 of
    2 ->
      Next
    0 ->
      Previous
    _ ->
      NoOp

movement: Signal Action
movement =
  Signal.map directionToAction direction

actions: Mailbox Action
actions =
  mailbox Reset

init: Signal.Mailbox String
init =
  Signal.mailbox ""

initiated: Signal Action
initiated =
  Signal.map Init init.signal

received : Signal.Mailbox String
received =
  Signal.mailbox ""

cardinput : Signal Action
cardinput =
  Signal.map Start received.signal

cardAndKeyboardInputs : Signal Action
cardAndKeyboardInputs =
  Signal.mergeMany [initiated, cardinput, movement]

allinputs : Signal Action
allinputs =
  Signal.merge actions.signal cardAndKeyboardInputs

clock: Signal Action
clock =
  Signal.map UpdateClock (every second)

signalsWithClock : Signal Action
signalsWithClock =
  Signal.merge allinputs clock

model: Signal Model
model =
  foldp update initialModel signalsWithClock

-- VIEW

monetize: Float -> Html
monetize number =
  text ((toString number) ++ "€")

previous: Address Action -> Model -> Html
previous address model =
  li [ onClick address Previous ] [ text "Prev" ]

next: Address Action -> Model -> Html
next address model =
  li [ onClick address Next ] [ text "Next" ]

nextButton: Address Action -> Model -> Html
nextButton address model =
      button [ onClick address Next, class "overlay next"]
        [
          p []
          [ phrase "next" model.language ]
        ]

prevButton: Address Action -> Model -> Html
prevButton address model =
        button [ onClick address Previous, class "overlay previous"]
        [
          p []
          [ phrase "previous" model.language ]
        ]

languageSelector: Address Action -> Model -> String -> Html
languageSelector address model language =
          div [ class "main_item -in_two" ]
          [
            div [ class "content" ]
            [
              p [ class "title" ]
              [ phrase "welcome" language ]
              ,
              button [ onClick address (Language language), class "main_button" ]
              [
                p []
                [ phrase "language" language ]
              ]
            ]
          ]


intro: Address Action -> Model -> Html
intro address model =
      div [ class "back -main" ]
      [
        div [ class "main_item -in_two" ]
        [
          p [ class "content" ]
          [ phrase "place_chip" "de" ]
        ]
        ,
        div [ class "main_item -in_two" ]
        [
          p [ class "content" ]
          [ phrase "place_chip" "en" ]
        ]

      ]

showClock: Address Action -> Model -> Html
showClock address model =
  div []
    [text (toString (model.seconds) )]


content: Address Action -> Model -> Html
content address model =
  case model.page of
    (-1) ->
      div []
      [
        div[]
          [text "server not started" ]
        ,
        div[]
          [text (toString model) ]
      ]
    0 ->
      intro address model
    1 ->
      div [ class "back -second" ]
      [
        languageSelector address model "de"
        ,
        languageSelector address model "en"
      ]
      -- Consider adding here some graphic / feedback about data received
    2 ->
      div [ class "back -second" ]
      [
        div [ class "main_item" ]
        [
          div [ class "content" ]
          [
            p [ class "title" ]
            [ phrase "total_duration" model.language ]
            ,
            -- just show if more than 60 minutes:
            p [ class "lead" ]
            [
              span [ class "big_number" ]
              [ text (toString model.visitorData.workingTime.hours) ]
              ,
              -- change phrase depending on singular or plural:
              phrase "duration_hours" model.language
            ]
            ,
            -- just show if minutes are not 0
            p [ class "lead"]
            [
              span [ class "big_number"]
              [ text (toString model.visitorData.workingTime.minutes) ]
              ,
              -- change phrase depending on singular or plural:
              phrase "duration_minutes" model.language
            ]
          ]
        ]
        ,
        -- if there are not data for next slide skip and go to slide 4
        nextButton address model
      ]
    3 ->
      div [ class "back -second" ]
      [
        div [ class "main_item" ]
        [
          div [ class "content" ]
          [
            p [ class "title" ]
            [ phrase "duration_by_area" model.language ]
            ,
            p [ class "panel -needs" ]
            [
              span [ class "panel_title" ]
              [ phrase "needs" model.language ]
              ,
              span [class "big_number" ]
              [ text "10" ]
              ,
              span [class "panel_time" ]
              [ phrase "duration_minutes" model.language ]
            ]
            ,
            p [ class "panel -mechanisms" ]
            [
              span [ class "panel_title" ]
              [ phrase "mechanisms" model.language ]
              ,
              span [class "big_number" ]
              [ text "23" ]
              ,
              span [class "panel_time" ]
              [ phrase "duration_minutes" model.language ]
            ]
            ,
            p [ class "panel -competition" ]
            [
              span [ class "panel_title" ]
              [ phrase "competition" model.language ]
              ,
              span [class "big_number" ]
              [ text "35" ]
              ,
              span [class "panel_time" ]
              [ phrase "duration_minutes" model.language ]
            ]
            ,
            p [ class "panel -colonialism"]
            [
              span [ class "panel_title"]
              [ phrase "colonialism" model.language ]
              ,
              span [class "big_number"]
              [ text "5"]
              ,
              span [class "panel_time"]
              [ phrase "duration_minutes" model.language]
            ]
            ,
            p [ class "panel -beyond"]
            [
              span [ class "panel_title"]
              [ phrase "beyond" model.language ]
              ,
              span [class "big_number"]
              [ text "110"]
              ,
              span [class "panel_time"]
              [ phrase "duration_minutes" model.language]
            ]
          ]
        ]
        ,
        prevButton address model
        ,
        nextButton address model
      ]
    4 ->
      div [ class "back -second" ]
      [
        div [ class "main_item" ]
        [
          div [ class "content" ]
          [
            p [ class "title" ]
            [ phrase "compared_visit" model.language ]
          ]
        ]
        ,
        prevButton address model
        ,
        nextButton address model
      ]
    5 ->
      div [ class "back -second" ]
      [
        div [ class "main_item"]
        [
          div [ class "content" ]
          [
            p [ class "title" ]
            [ phrase "lost" model.language ]
            ,
            p [ class "lead -centered" ]
            [
              span []
              [ phrase "lost_explanation_p1" model.language ]
              ,
              span [ class "emph_number" ]
              [ text "115" ]
              ,
              span []
              [ phrase "lost_explanation_p2" model.language ]
            ]
            ,
            p [ class "prof_panel -football" ]
            [
              span [ class "panel_title" ]
              [ phrase "football_t" model.language ]
              ,
              span [class "big_number" ]
              [ monetize 9143 ]
              ,
              span [class "panel_medium" ]
              [ phrase "football_p" model.language  ]
            ]
            ,
            p [ class "prof_panel -doctor" ]
            [
              span [ class "panel_title" ]
              [ phrase "doctor_t" model.language ]
              ,
              span [class "panel_number" ]
              [ monetize 176 ]
              ,
              span [class "panel_small" ]
              [ phrase "doctor_p" model.language ]
            ]
            ,
            p [ class "prof_panel -manager" ]
            [
              span [ class "panel_title" ]
              [ phrase "manager_t" model.language ]
              ,
              span [class "panel_number" ]
              [ monetize 155 ]
              ,
              span [class "panel_small" ]
              [ phrase "manager_p" model.language ]
            ]
            ,
            p [ class "prof_panel -pilot" ]
            [
              span [ class "panel_title" ]
              [ phrase "pilot_t" model.language ]
              ,
              span [class "panel_number" ]
              [ monetize 123 ]
              ,
              span [class "panel_small" ]
              [ phrase "pilot_p" model.language ]
            ]
            ,
            p [ class "prof_panel -judge" ]
            [
              span [ class "panel_title" ]
              [ phrase "judge_t" model.language ]
              ,
              span [class "panel_number" ]
              [ monetize 120 ]
              ,
              span [class "panel_small" ]
              [ phrase "judge_p" model.language ]
            ]
            ,
            p [ class "prof_panel -teacher" ]
            [
              span [ class "panel_title" ]
              [ phrase "teacher_t" model.language ]
              ,
              span [class "panel_number" ]
              [ monetize 111 ]
              ,
              span [class "panel_small" ]
              [ phrase "teacher_p" model.language ]
            ]
            ,
            p [ class "prof_panel -mechanic" ]
            [
              span [ class "panel_title" ]
              [ phrase "mechanic_t" model.language ]
              ,
              span [class "panel_number" ]
              [ monetize 79 ]
              ,
              span [class "panel_small" ]
              [ phrase "mechanic_p" model.language ]
            ]
            ,
            p [ class "prof_panel -salesman" ]
            [
              span [ class "panel_title" ]
              [ phrase "salesman_t" model.language ]
              ,
              span [class "panel_number" ]
              [ monetize 73 ]
              ,
              span [class "panel_small" ]
              [ phrase "salesman_p" model.language ]
            ]
            ,
            p [ class "prof_panel -nurse" ]
            [
              span [ class "panel_title" ]
              [ phrase "nurse_t" model.language ]
              ,
              span [class "panel_number" ]
              [ monetize 63 ]
              ,
              span [class "panel_small" ]
              [ phrase "nurse_p" model.language ]
            ]
            ,
            p [ class "prof_panel -hairdresser" ]
            [
              span [ class "panel_title" ]
              [ phrase "hairdresser_t" model.language ]
              ,
              span [class "panel_number" ]
              [ monetize 22 ]
              ,
              span [class "panel_small" ]
              [ phrase "hairdresser_p" model.language ]
            ]
            ,
            p [ class "prof_panel -student" ]
            [
              span [ class "panel_title" ]
              [ phrase "student_t" model.language ]
              ,
              span [class "panel_number" ]
              [ monetize 5 ]
              ,
              span [class "panel_small" ]
              [ phrase "student_p" model.language ]
            ]
            ,
            p [ class "sources"]
            [ phrase "sources" model.language ]
          ]
        ]
        ,
        prevButton address model
        ,
        nextButton address model
      ]
    6 ->
      div [ class "back -second" ]
      [
        div [ class "main_item" ]
        [
          div [ class "content" ]
          [
            p [ class "title" ]
            [ phrase "point" model.language ]
            ,
            p [ class "lead" ]
            [ phrase "point_explanation" model.language ]
            ,
            button [ onClick address (Subpage 1), class "main_button -medium -comparability" ]
            [
              p []
              [ phrase "comparability" model.language ]
            ]
            ,
            button [ onClick address (Subpage 2), class "main_button -medium -exploitation" ]
            [
              p []
              [ phrase "exploitation" model.language ]
            ]
            ,
            button [ onClick address (Subpage 3), class "main_button -medium -money"]
            [
              p []
              [ phrase "money" model.language ]
            ]
          ]
        ]
        ,
        prevButton address model
        ,
        nextButton address model
      ]
    601 ->
      div [ class "back -second" ]
      [
        div [ class "main_item -comparability"]
        [
          div [ class "content" ]
          [
            p [ class "title" ]
            [ phrase "comparability" model.language ]
            ,
            p [ class "readable" ]
            [ phrase "comparability_p1" model.language ]
            ,
            p [ class "readable" ]
            [ phrase "comparability_p2" model.language ]
          ]
        ]
        ,
        button [ onClick address LeaveSubpage, class "overlay menu" ]
        [
          p []
          [ phrase "leave_subpage" model.language ]
        ]
      ]
    602 ->
      div [ class "back -second" ]
      [
        div [ class "main_item -exploitation"]
        [
          div [ class "content" ]
          [
            p [ class "title" ]
            [ phrase "exploitation" model.language ]
            ,
            p [ class "readable" ]
            [ phrase "exploitation_p1" model.language ]
            ,
            p [ class "readable" ]
            [ phrase "exploitation_p2" model.language ]
          ]
        ]
        ,
        button [ onClick address LeaveSubpage, class "overlay menu" ]
        [
          p []
          [ phrase "leave_subpage" model.language ]
        ]
      ]
    603 ->
      div [ class "back -second" ]
      [
        div [ class "main_item -money"]
        [
          div [ class "content" ]
          [
            p [ class "title" ]
            [ phrase "money" model.language ]
            ,
            p [ class "readable" ]
            [ phrase "money_p1" model.language ]
          ]
        ]
        ,
        button [ onClick address LeaveSubpage, class "overlay menu" ]
        [
          p []
          [ phrase "leave_subpage" model.language ]
        ]
      ]
    7 ->
      div [ class "back -second" ]
      [
        div [ class "main_item"]
        [
          div [ class "content" ]
          [
            p [ class "title" ]
            [ phrase "thankyou" model.language ]
            ,
            p [ class "readable" ]
            [ phrase "thankyou_p1" model.language ]
            ,
            p [ class "readable" ]
            [ phrase "thankyou_p2" model.language ]
            ,
            showClock address model
          ]
        ]
      ]
    _ ->
      div []
        [
          div []
          [text "error: unkonw page" ]
          ,
          div []
          [ text (toString model.page) ]
        ]
view : Address Action -> Model -> Html
view address model =
  content address model


-- MAIN

main : Signal Html
main =
  map (view actions.address) model
