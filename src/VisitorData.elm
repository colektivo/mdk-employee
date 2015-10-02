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
--   stats: [],
--   timeReport: [],
--   isValid: false,
--   isComplete: false }

type alias WorkingTime = { years: Int, months: Int, days: Int, hours: Int, minutes: Int, seconds: Int, decimalTime: Float }
type alias TimeSpent =  { hours: Maybe Int, minutes: Maybe Int, seconds: Maybe Int }
type alias Report = { position: Int, timeSpent: TimeSpent, timeSpentInSeconds : Float }
type alias VisitorData = { workingTime : WorkingTime, timeReport : List Report, stats: Maybe (List Stat), isValid: Bool, isComplete: Bool, salaries : Maybe (List Payment) }
type alias Payment = { text: String, income: Float, payment : Float }
type alias Stat = { position: Int, averageTimeSpent : Int, records: Int }

decodeVisitorData: String -> Result String VisitorData
decodeVisitorData =
  Decode.decodeString <| Decode.object6 VisitorData
    ( "workingTime" := decodeWorkingTime )
    ( "timeReport" := Decode.list decodeReport )
    ( Decode.maybe ("stats" := Decode.list decodeStat) )
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

decodeStat: Decode.Decoder Stat
decodeStat =
    Decode.object3 Stat
      ( "position" := Decode.int )
      ( "averageTimeSpent" := Decode.int )
      ( "records" := Decode.int )


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
  , stats = Just
    [ { position = 1 , averageTimeSpent = 1800, records = 100 }
    , { position = 2 , averageTimeSpent = 3600, records = 100  }
    , { position = 3 , averageTimeSpent = 1800, records = 100  }
    , { position = 4 , averageTimeSpent = 3600, records = 100  }
    , { position = 5 , averageTimeSpent = 1800, records = 100  }
    , { position = 0 , averageTimeSpent = 12600, records = 100  }
    ]
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
      { text =  "football_t"          , income =  830000  , payment = 18154.5 }
     ,{ text =  "doctor_t"            , income =  16000   , payment = 350.0   }
     ,{ text =  "manager_t"           , income =  11100   , payment = 241.5   }
     ,{ text =  "pilot_t"             , income =  10300   , payment = 224.0   }
     ,{ text =  "judge_t"             , income =  6100    , payment = 133.0   }
     ,{ text =  "teacher_t"           , income =  4030    , payment = 87.5    }
     ,{ text =  "mechanic_t"          , income =  2830    , payment = 59.5    }
     ,{ text =  "salesman_t"          , income =  2576    , payment = 56.0    }
     ,{ text =  "nurse_t"             , income =  2064    , payment = 42.0    }
     ,{ text =  "hairdresser_t"       , income =  1288    , payment = 28.0    }
     ,{ text =  "student_t"           , income =  670     , payment = 14.0    }
    ]
  }
