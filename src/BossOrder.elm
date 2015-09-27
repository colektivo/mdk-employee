module BossOrder where

import Graphics.Element exposing (show)
import Task             exposing (Task, andThen)
import String
import SocketIO
import Html             exposing (..)
import Html.Attributes  exposing (..)
import Html.Events      exposing (onClick)
import Signal           exposing (..)
import Json.Encode      as Encode
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode

--
--
-- TYPES
--
--

type alias BossMessage =
    { command: String
    , config: Maybe Config
    }

type alias Config =
  { devices: List Device
  , deviceData: Maybe DeviceData
  , message: Maybe String
  }

type alias Device = { position: Int, device: String }

defaultConfig : Config
defaultConfig =
    { devices = []
    , deviceData = Nothing
    , message = Just ""
  }

defaultMessage : BossMessage
defaultMessage =
  {
      command = "NoOp"
    , config = Just defaultConfig
  }

defaultDeviceData : DeviceData
defaultDeviceData =
  { devicePath = "No Device"
  , id = "No Id"
  }


-- socket : Task x SocketIO.Socket
-- socket = SocketIO.io "http://localhost:8001" SocketIO.defaultOptions

--
--
-- INCOMING MESSAGES
--
--

-- port followBoss : Task x ()
-- port followBoss = socket `andThen` SocketIO.on "boss says" bossSays.address

bossSays: Signal.Mailbox String
bossSays =
  Signal.mailbox ""

decodeMessage : String -> Result String BossMessage
decodeMessage =
  Decode.decodeString <| Decode.object2 BossMessage
    ("command" := Decode.string )
    ( Decode.maybe ("config" := decodeConfig ) )

decodeConfig: Decode.Decoder Config
decodeConfig =
  Decode.object3 Config
    ( "devices" := Decode.list decodeDevice )
    ( Decode.maybe ("readerData" := decodeDeviceData ) )
    ( Decode.maybe ("message" := Decode.string ) )

decodeDevice: Decode.Decoder Device
decodeDevice =
    Decode.object2 Device
      ("position" := Decode.int )
      ("device" := Decode.string )

toValidBossMessage : Result String BossMessage -> BossMessage
toValidBossMessage result =
  case result of
    Ok value -> value
    _ -> defaultMessage

incomingMessages : Signal BossMessage
incomingMessages =
  let
    messages = Signal.map decodeMessage bossSays.signal
  in
    Signal.map toValidBossMessage messages

-- The data that is received from the backend

-- data:
--   { "devicePath" : "USB_08ff_0009_14534400"
--   , "id": "3504675323"
--   }

type alias DeviceData = { devicePath: String, id: String }

decodeDeviceData : Decode.Decoder DeviceData
decodeDeviceData =
  Decode.object2 DeviceData
    ("devicePath" := Decode.string )
    ("id" := Decode.string )

toVaildDeviceData data = Maybe.withDefault defaultDeviceData data

--
--
-- OUTGOING ENCODING
--
--

-- port talkToBoss : Signal (Task x ())
-- port talkToBoss = employeeMessages

configToJson config = Encode.encode 0 <| encodeConfig config
encodeConfig {devices} = Encode.object [ ("devices", Encode.list (List.map encodeDevice devices) )]
encodeDevice {device, position} = Encode.object [ ("device", Encode.string device),  ("position", Encode.int position ) ]

toValidConfig config = Maybe.withDefault defaultConfig config

-- bossSignals = Signal.merge outgoingMessages.signal incomingMessages

-- main =
--   Signal.map show incomingMessages
