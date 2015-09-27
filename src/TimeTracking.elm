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
import BossOrder        exposing (..)
import Json.Encode      as Encode
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Debug

-- BOSS

socket : Task x SocketIO.Socket
socket = SocketIO.io "http://localhost:8001" SocketIO.defaultOptions

--
-- Ports from BossOrder used to communicate to the Boss
--

port followBoss : Task x ()
port followBoss = socket `andThen` SocketIO.on "boss says" (Debug.log "bossSays.address:" bossSays.address)

port talkToBoss : Signal (Task x ())
port talkToBoss = employeeMessages

employeeMessages : Signal (Task x ())
employeeMessages =
  let
    send message = socket `andThen` SocketIO.emit (Debug.log "message" message.command) (configToJson (toValidConfig message.config))
    bossMessagesFromView = Signal.map actionToBoss signalsWithClock
    bossMessages = Signal.merge outgoingMessages.signal bossMessagesFromView
  in
    Signal.map send bossMessages

outgoingMessages: Signal.Mailbox BossMessage
outgoingMessages =
  Signal.mailbox startMessage

-- messages port where we will receive the visitorData
port responses : Task x ()
port responses = socket `andThen` SocketIO.on "completed" received.address

-- MODEL

type alias Model =
  { visitorData: VisitorData
  , commands: List String
  , messages: List Message
  , config: Config
  , currentDeviceCheck: DeviceData
  , currentDevicePosition: Int
  , validatedDevices: List Device
  , confirmedDevices: List (Device, Bool)
  , page: Int
  , timeoutOnPage: Int
  , timeoutInSeconds: Int
  , seconds: Int
  , language: String
  , readyToStart: Bool
  , configurationSaved: Bool
  }

initialModel : Model
initialModel =
  { visitorData = defaultCardData
  , messages = []
  , commands = []
  , config = defaultConfig
  , currentDeviceCheck = defaultDeviceData
  , currentDevicePosition = 1
  , validatedDevices = []
  , confirmedDevices = []
  , timeoutInSeconds = 600
  , timeoutOnPage = 7
-- page -1 server not initiated
  , page = -4
  , seconds = 0
  , language = "en"
  , readyToStart = False
  , configurationSaved = False
  }

-- ACTIONS

startMessage : BossMessage
startMessage =
  { command = "init"
  , config = Nothing
  }

startTracking : BossMessage
startTracking =
  { command = "start"
  , config = Nothing
  }

restartMessage : BossMessage
restartMessage =
  { command = "restart"
  , config = Nothing
  }

saveConfiguration model =
  {
    command = "save config"
  , config = Just model.config
  }

type Action =   Next
            | RestartConfiguration
            | CheckDevices Model
            | Ready (Maybe Config)
            | Start
            | Check (Maybe DeviceData)
            | Configure (Maybe Config)
            | Say BossMessage
            | Connect
            | Init String
            | Previous
            | Reset
            | Card String
            | Language String
            | Subpage Int
            | LeaveSubpage
            | NoOp
            | UpdateClock Float

-- UPDATE

handleTimeout : Model -> Model
handleTimeout log =

    case log.page of
      -- configuration page
      (-4) ->
        let
          timeToStart =
            log.page == -4 && log.seconds > 5
        in
          if timeToStart then
            log
          else { log | seconds <- log.seconds + 1 }
      7 ->
        if log.seconds > log.timeoutInSeconds then update Reset log else { log | seconds <- log.seconds + 1 }
      _ ->
        { log | seconds <- log.seconds + 1 }


setDevicesToValidate config =
  List.map (\dev -> { dev | device <- ""}) config.devices

setDevicesToConfirm validatedDevices =
  List.map (\dev -> (dev, False)) validatedDevices


setReceivedDevice: Model -> DeviceData -> List Device
setReceivedDevice model readerData =
  let
    -- check that does not exist
    currentReceivedDevices = List.map (\received -> received.device) model.validatedDevices
    alreadyExisting = List.member readerData.devicePath currentReceivedDevices
    --
    emptyDevices = List.filter (\received -> received.device == "") model.validatedDevices
    alreadyReceived = List.filter (\received -> received.device /= "") model.validatedDevices
    currentPosition = (List.length alreadyReceived) + 1
    nextToReceive = List.take 1 emptyDevices
    notYetReceived = List.drop 1 emptyDevices
    newDevice = List.map (\emptyDevice -> { emptyDevice | device <- readerData.devicePath, position <- currentPosition } ) nextToReceive
  in
    if alreadyExisting
      then
        model.validatedDevices
      else
        List.concat [alreadyReceived, newDevice, notYetReceived]

isAllConfigured validatedDevices =
  List.all (\device -> device.device /= "") validatedDevices

deviceToConfirm confirmedDevices =
  let
    sortedDevices = List.sortBy (\(d,b) -> d.position) confirmedDevices
  in
    List.take 1 ( List.filter (\(_, confirmed)-> confirmed == False ) sortedDevices)

allConfirmed confirmedDevices =
  List.all (\(device, confirmed) -> confirmed ) confirmedDevices

isConfirmed model readerData =
  List.all (\(device, confirmed) -> (Debug.log "checking device" device.device) == readerData.devicePath ) (deviceToConfirm model.confirmedDevices)

confirm (device, confirmed) devicePath =
  if device.device == devicePath
    then
      (device, True)
    else
      (device, confirmed)


confirmedDevicesWith model readerData =
  List.map (\(device, confirmed) -> confirm (device, confirmed) readerData.devicePath ) model.confirmedDevices

updateConfig confirmedDevices =
  { devices = (List.map (\(d,_)-> d ) confirmedDevices)
    , deviceData = Nothing
    , message = Nothing
  }

update : Action -> Model -> Model
update action log =
  case action of
    CheckDevices model ->
      Debug.log "checkDevices:" log
    Check readerData ->
      case log.page of
        -- Device position discovery page
        (-3) ->
          let
            validDeviceData= toVaildDeviceData readerData
            devicePath = validDeviceData.devicePath
            currentValidatedDevices = setReceivedDevice log (toVaildDeviceData readerData)
          in
            { log | currentDevicePosition <- log.currentDevicePosition + 1
                  , currentDeviceCheck <- (Debug.log "reader" (toVaildDeviceData readerData))
                  , validatedDevices <- currentValidatedDevices
                  , confirmedDevices <- setDevicesToConfirm currentValidatedDevices
                  , page <- if isAllConfigured (setReceivedDevice log (toVaildDeviceData readerData)) then -2 else log.page
            }
        -- Device position validation page
        (-2) ->
          let
            isCurrentDeviceConfirmed = isConfirmed log (toVaildDeviceData readerData)
            currentConfirmedDevices = confirmedDevicesWith log (toVaildDeviceData readerData)
            isComplete = allConfirmed currentConfirmedDevices
          in
            { log | page <- if isCurrentDeviceConfirmed
                            then
                              -2
                            else
                              -3
                  , confirmedDevices <- if isCurrentDeviceConfirmed then currentConfirmedDevices else setDevicesToConfirm log.validatedDevices
                  , validatedDevices <- if isCurrentDeviceConfirmed then log.validatedDevices else setDevicesToValidate log.config
                  , config <- if isComplete then updateConfig currentConfirmedDevices else log.config
                  , readyToStart <-isComplete
            }
        otherwise ->
          log
    Ready newConfig ->
      { log | page <- -1, seconds <- 0, config <- (Debug.log "config:" toValidConfig newConfig)  }
    Say message ->
      log
    NoOp ->
      log
    Configure config ->
      { log | config <- (toValidConfig config)
            , validatedDevices <- setDevicesToValidate (toValidConfig config)
            , page <- -3
            , seconds <- 0 }
    UpdateClock _ ->
      handleTimeout log
    Previous ->
      { log | page <- log.page - 1, seconds <- 0 }
    Next ->
      { log | page <- log.page + 1, seconds <- 0 }
    Card data ->
      { log | visitorData <- toValidVisitorData ( decodeVisitorData data ) , page <- 1 }
    Start ->
      { log | visitorData <- defaultCardData , page <- 0, seconds <- 0 }
    Reset ->
      { log | visitorData <- defaultCardData , page <- 0, seconds <- 0 }
    Language language ->
      { log | language <- language, page <- 2}
    Subpage subpage ->
      { log | page <- 600 + subpage}
    LeaveSubpage ->
      { log | page <- 6}
    _ ->
      log

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

actions: Mailbox Action
actions =
  mailbox Reset

commandToAction : BossMessage -> Action
commandToAction bossMessage =
  let command = (Debug.log "bossMessage.command:" bossMessage.command)
  in
    case command of
      "configure" ->
        Configure (Debug.log "config" bossMessage.config)
      "ready to start" ->
        Ready bossMessage.config
      "check" ->
        let
          config = toValidConfig bossMessage.config
          deviceData = config.deviceData
        in
          Check (Debug.log "readerData" deviceData)
      _ ->
        NoOp

actionToBoss : Action -> BossMessage
actionToBoss action  =
  case action of
    Say message ->
      Debug.log "actionToBoss:" message
    CheckDevices model ->
      Debug.log "check devices:" { command = (Debug.log "start:" "start")
        , config = Just model.config
      }

    _ ->
      defaultMessage


outMessages : Signal BossMessage
outMessages =
  let
    isCommand action =
      case action of
        Say _ ->
          True
        _     ->
          False
    onlyCommands = Signal.filter isCommand (NoOp) signalsWithClock
  in
    Signal.map actionToBoss onlyCommands


received : Signal.Mailbox String
received =
  Signal.mailbox ""

cardinput : Signal Action
cardinput =
  Signal.map Card received.signal

signalsWithClock : Signal Action
signalsWithClock =
  let
    movement = Signal.map directionToAction direction
    cardAndKeyboardInputs = Signal.merge cardinput movement
    allinputs = Signal.merge actions.signal cardAndKeyboardInputs
    boss = Signal.map commandToAction incomingMessages
    allInputsWithBoss = Signal.merge boss allinputs
    clock = Signal.map UpdateClock (every second)
  in
    Signal.merge allInputsWithBoss clock

model: Signal Model
model =
  foldp update initialModel signalsWithClock

--
--
-- VIEW
--
-- monetize (paymentEntry "Nurse" model.salaries)

paymentEntry : String -> List Payment -> Maybe Payment
paymentEntry text salaries =
  List.head (List.filter (\element -> element.text == text) salaries)

paymentAmount : Payment -> Html
paymentAmount comparison =
  monetize (comparison.payment)

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

restartButton: Address Action -> Model -> Html
restartButton address model =
        button [ onClick address (Say restartMessage), class "overlay previous"]
        [
          p []
          [ phrase "restart configuration" model.language ]
        ]


saveConfigButton: Address Action -> Model -> Html
saveConfigButton address model =
        if model.readyToStart then
          button [ onClick address (Say (saveConfiguration model)), class "overlay next"]
          [
            p []
            [ phrase "save configuration" model.language ]
          ]
        else
          p []
            [ text "..." ]


twoButtonSelector : Address Action -> Model -> String -> String -> Action -> String -> Html
twoButtonSelector address model title actionText action language =
          div [ class "main_item -in_two" ]
          [
            div [ class "content" ]
            [
              p [ class "title" ]
              [ phrase title language ]
              ,
              button [ onClick address action , class "main_button" ]
              [
                p []
                [ phrase actionText language ]
              ]
            ]
          ]


languageSelector: Address Action -> Model -> String -> Html
languageSelector address model language =
          twoButtonSelector address model "welcome" "language" (Language language) language

startOrConfigure: Address Action -> Model -> Html
startOrConfigure address model =
      div [ class "back -second" ]
      [
        twoButtonSelector address model "review config" "configure" (Configure Nothing) "en"
        ,
        twoButtonSelector address model "start tracking" "start" (Say startTracking) "en"
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

listDevices : List Device -> Html
listDevices devices =
  let
    devicesWithIndex = List.indexedMap (,) devices
    nonZeroPositions (index, device) =
      if device.position == 0 then (index + 1, device.device)
        else (device.position, device.device)
    sortedDevicePositions =
      List.sort (List.map nonZeroPositions devicesWithIndex)
  in
    ul []
      (List.map deviceEntry sortedDevicePositions)

deviceEntry: (Int,String) -> Html
deviceEntry position =
  li []
    [ text (toString position) ]


container : List Html -> Html
container html =
  div [ class "main_item" ]
    [
      div [ class "content" ]
        html
    ]

title : String -> Model -> Html
title key model =
  p [ class "title" ]
    [ phrase key model.language ]


content: Address Action -> Model -> Html
content address model =
  case model.page of
    (-4) ->
      div [ class "back -second" ]
      [
        container [
          title "start the server" model
        ]
      ]
    (-3) ->
      div [ class "back -second" ]
      [
        container [
          title "configure devices and positions" model
          ,
          div[]
            [ text (toString model.validatedDevices)]
          ,
          div[]
            [ text (toString model.confirmedDevices)]
          ,

--          div[]
--            [ text (toString (isAllConfigured model))]
--          ,


--          div[]
--            [ phrase "configuration" "en" ]
--          ,
--          div[]
--            [text (toString model.config) ]
--          ,
--          div[]
--            [text (toString model.currentDeviceCheck) ]
--          ,
--          div[]
--            [text (toString model.validatedDevices) ]
--          ,
--          div[]
--            [text (toString model.currentDevicePosition) ]
--          ,
          div[]
            [ listDevices model.config.devices ]
          ,
          restartButton address model
        ]
      ]

    (-2) ->
      div [ class "back -second" ]
        [
          container [
            title "validate devices and positions" model
            ,
            div[]
              [ text (toString (deviceToConfirm model.confirmedDevices))]
            ,
            div[]
              [ text (toString model.confirmedDevices)]
            ,
            div[]
              [ listDevices model.config.devices ]
            ]
            ,
            saveConfigButton address model
        ]
    -- ready to start
    (-1) ->
      startOrConfigure address model
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
        container [
          title "total_duration" model
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
        -- if there are not data for next slide skip and go to slide 4
          ]
        ,
        nextButton address model
      ]
    3 ->
      div [ class "back -second" ]
      [
        container [
            title "duration_by_area" model
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
        ,
        prevButton address model
        ,
        nextButton address model
      ]
    4 ->
      div [ class "back -second" ]
      [
        container
          [
            title "compared_visit" model
          ]
        ,
        prevButton address model
        ,
        nextButton address model
      ]
    5 ->
      div [ class "back -second" ]
      [
        container [
          title "lost" model
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
        ,
        prevButton address model
        ,
        nextButton address model
      ]
    6 ->
      div [ class "back -second" ]
      [
        container [
          title "point" model
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
            title "comparability" model
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
            title "exploitation" model
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
            title "money" model
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
            title "thankyou" model
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
  Signal.map (view actions.address) model
