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
type alias TimeSpent =  { hours: Maybe Int, minutes: Maybe Int, seconds: Maybe Int }
type alias Report = { position: Int, timeSpent: TimeSpent, timeSpentInSeconds : Float }
type alias VisitorData = { workingTime : WorkingTime, timeReport : List Report, isValid: Bool, isComplete: Bool, salaries : Maybe (List Payment) }
type alias Payment = { text: String, income: Float, payment : Float }

decodeVisitorData: String -> Result String VisitorData
decodeVisitorData =
  Decode.decodeString <| Decode.object5 VisitorData
    ( "workingTime" := decodeWorkingTime )
    ( "timeReport" := Decode.list decodeReport )
    ( "isValid" := Decode.bool )
    ( "isComplete" := Decode.bool )
    ( Decode.maybe ("salaries" := Decode.list decodePayment) )

toValidVisitorData : Result String VisitorData -> VisitorData
toValidVisitorData result =
  case result of
    Ok value -> value
    _ -> defaultCardData

decodePayment : Decode.Decoder Payment
decodePayment =
  Decode.object3 Payment
    ("text" := Decode.string )
    ("income" := Decode.float )
    ("payment" := Decode.float )

decodeReport: Decode.Decoder Report
decodeReport =
  Decode.object3 Report
    ( "position" := Decode.int )
    ( "timeSpent" := decodeTimeSpent )
    ( "timeSpentInSeconds" := Decode.float )

decodeTimeSpent: Decode.Decoder TimeSpent
decodeTimeSpent =
  Decode.object3 TimeSpent
    ( Decode.maybe ("hours" := Decode.int ) )
    ( Decode.maybe ("minutes" := Decode.int ) )
    ( Decode.maybe ("seconds" := Decode.int ) )

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

defaultCardData : VisitorData
defaultCardData =
  { isComplete = True
  , isValid = True
  , timeReport =
    [ { position = 1, timeSpent = { hours = Just 1, minutes = Nothing, seconds = Nothing }, timeSpentInSeconds = 3600 }
    , { position = 2, timeSpent = { hours = Nothing, minutes = Just 30, seconds = Just 10 }, timeSpentInSeconds = 1810 }
    , { position = 3, timeSpent = { hours = Nothing, minutes = Just 30, seconds = Just 1 }, timeSpentInSeconds = 1801 }
    , { position = 4, timeSpent = { hours = Just 1, minutes = Nothing, seconds = Just 1 }, timeSpentInSeconds = 3601 }
    , { position = 5, timeSpent = { hours = Nothing, minutes = Just 30, seconds = Just 1 }, timeSpentInSeconds = 1801 }
    ]
  , workingTime =
    { years = 0
    , months = 0
    , days = 0
    , decimalTime = 3.5
    , hours = 3
    , seconds = 13
    , minutes = 30
    }
  , salaries = Just [
      { text =  "Philip Lahm"          , income =  830000  , payment = 18154.5 }
     ,{ text =  "Chief physician"      , income =  16000   , payment = 350.0   }
     ,{ text =  "Manager"              , income =  11100   , payment = 241.5   }
     ,{ text =  "Pilot"                , income =  10300   , payment = 224.0   }
     ,{ text =  "Judge"                , income =  6100    , payment = 133.0   }
     ,{ text =  "School teacher"       , income =  4030    , payment = 87.5    }
     ,{ text =  "Car mechanic"         , income =  2830    , payment = 59.5    }
     ,{ text =  "Insurance salesman"   , income =  2576    , payment = 56.0    }
     ,{ text =  "Nurse"                , income =  2064    , payment = 42.0    }
     ,{ text =  "Hairdresser"          , income =  1288    , payment = 28.0    }
     ,{ text =  "Student"              , income =  670     , payment = 14.0    }
    ]
  }
