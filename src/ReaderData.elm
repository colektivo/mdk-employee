module ReaderData where

import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode

-- The data that is received from the backend

-- data:
-- { "checkpoint" :
--   { "devicePath" : "USB_08ff_0009_14534400"
--   , "reader": { "counter":0
--               , "fullBuffer":[]
--               ,"_events":{}
--               }
--  , "device":  { "domain": null
--               , "_events": {}
--               , "_maxListeners": 10
--               , "_raw": {}
--               , "_paused": false }
--  ,"up": true }
--  ,"id": "3504675323"
-- }

type alias CheckPointData = { checkpoint: DeviceData }
type alias DeviceData = { devicePath: String, id: String }

decodeReaderData: String -> Result String CheckPointData
decodeReaderData =
  Decode.decodeString <| Decode.object1 CheckPointData
    ( "checkpoint" := decodeDeviceData )

decodeDeviceData : Decode.Decoder DeviceData
decodeDeviceData =
  Decode.object2 DeviceData
    ("devicePath" := Decode.string )
    ("id" := Decode.string )
