module VisitorData where

import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode

-- The data that is received from the backend

-- { workingTime:
--    { years: 0,
--      months: 0,
--      days: 0,
--      hours: 0,
--      minutes: 0,
--      seconds: 0,
--      decimalTime: 0 },
--   timeReport: [],
--   isValid: false,
--   isComplete: false }

type alias WorkingTime = { years: Int, months: Int, days: Int, hours: Int, minutes: Int, seconds: Int, decimalTime: Float }
type alias TimeSpent =  { hours: Maybe Int, minutes: Maybe Int, seconds: Maybe Int, timeSpentInSeconds : Float }
type alias Report = { position: Int, timeSpent: TimeSpent }
type alias VisitorData = { workingTime : WorkingTime, timeReport : List Report, isValid: Bool, isComplete: Bool }

decodeVisitorData: String -> Result String VisitorData
decodeVisitorData =
  Decode.decodeString <| Decode.object4 VisitorData
    ( "workingTime" := decodeWorkingTime )
    ( "timeReport" := Decode.list decodeReport )
    ( "isValid" := Decode.bool )
    ( "isComplete" := Decode.bool )

decodeReport: Decode.Decoder Report
decodeReport =
  Decode.object2 Report
    ( "position" := Decode.int )
    ( "timeSpent" := decodeTimeSpent )

decodeTimeSpent: Decode.Decoder TimeSpent
decodeTimeSpent =
  Decode.object4 TimeSpent
    ( Decode.maybe ("hours" := Decode.int ) )
    ( Decode.maybe ("minutes" := Decode.int ) )
    ( Decode.maybe ("seconds" := Decode.int ) )
    ( "timeSpentInSeconds" := Decode.float )


workingTime : Decode.Decoder WorkingTime
workingTime =
  Decode.object7 WorkingTime
    ( "years" := Decode.int )
    ( "months" := Decode.int )
    ( "days" := Decode.int )
    ( "hours" := Decode.int )
    ( "minutes" := Decode.int )
    ( "seconds" := Decode.int )
    ( "decimalTime" := Decode.float )

decodeWorkingTime: Decode.Decoder WorkingTime
decodeWorkingTime =
  Decode.object7 WorkingTime
    ( "years" := Decode.int )
    ( "months" := Decode.int )
    ( "days" := Decode.int )
    ( "hours" := Decode.int )
    ( "minutes" := Decode.int )
    ( "seconds" := Decode.int )
    ( "decimalTime" := Decode.float )
