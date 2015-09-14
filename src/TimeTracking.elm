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

socket : Task x SocketIO.Socket
socket = SocketIO.io "http://localhost:8001" SocketIO.defaultOptions

port initial : Task x ()
port initial = socket `andThen` SocketIO.emit "" "Hello I am a browser using websockets"

port responses : Task x ()
port responses = socket `andThen` SocketIO.on "completed" received.address


-- MODEL

type alias Model =
  {  events: List String,
    page: Int,
    language: String
  }

initialModel : Model
initialModel =
  {
    events = [],
    page = 0,
    language = ""
  }


-- ACTIONS

type Action =   Next
          | Previous
          | Reset
          | Start String
          | Language String
          | Subpage Int
          | LeaveSubpage

-- UPDATE

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
      { log | events <- [], page <- 0 }
    Language language ->
      { log | language <- language, page <- 2}
    Subpage subpage ->
      { log | page <- 600 + subpage}
    LeaveSubpage ->
      { log | page <- 6}

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


-- VIEW

previous: Address Action -> Model -> Html
previous address model =
  li [ onClick address Previous ] [ text "Prev" ]

next: Address Action -> Model -> Html
next address model =
  li [ onClick address Next ] [ text "Next" ]

phrase: String -> String -> Html
phrase key language =
  case language of
    "de" ->
      case key of
        "welcome"               -> text "Willkommen!"
        "place_chip"            -> text "Bitte halte deine Chipkarte auf das Lesegerät um deine persönliche Auswertung zu erhalten"
        "total_duration"        -> text "Dauer deines Aufenthaltes im Museum"
        "duration_hours"        -> text "Stunde"
        "duration_minutes"      -> text "Minuten"
        "duration_by_area"      -> text "Wie lange warst du in jedem Themenbereich?"
        "needs"                 -> text "Needs and Exclusions"
        "mechanisms"            -> text "Mechanisms of Capitalism"
        "competition"           -> text "Competition and Crisis"
        "colonialism"           -> text "Colonialism"
        "beyond"                -> text "Beyond Capitalism"
        "compared_visit"        -> text "Dein Aufenthalt verglichen mit dem/der durchschnittlichen Besucher_in"
        "lost"                  -> text "Du hast gerade Geld und Zeit verloren"
        "lost_explanation_p1"   -> text "In "
        "lost_explanation_p2"   -> text " Minuten hättest du eine Menge Geld verdienen können, wenn du gearbeitet hättest anstatt das Museum zu besuchen"
        "point"                 -> text "Was soll das?"
        "point_explanation"     -> text "Berühre die Themen, um mehr zu erfahren"
        "comparability"         -> text "Vergleichbarkeit und Konkurrenz"
        "exploitation"          -> text "Verwertung von Informationen"
        "money"                 -> text "Geld und Anerkennung"
        "comparability_p1"      -> text "In kapitalistischen Gesellschaften sollen wir uns ständig vergleichen. Wettbewerbsfähigkeit wird in Effizienz gemessen, Konkurrenz ist gut fürs Geschäft – zwischen Unternehmen aber auch zwischen Mitarbeiter_innen. Sitzt du länger im Büro als deine Kolleg_innen, kommst du immer als erste_r? Die Technik, um all das zu erfassen und zu vergleichen, ist schon lange vorhanden. Wie lang warst du im Museum? Wie effizient hast du die Ausstellung besucht? Und ist Zeit dafür die richtige Kategorie?"
        "comparability_p2"      -> text "Dieses Exponat ist nur ein Beispiel dafür, dass Menschen im Kapitalismus nur noch Rädchen im Getriebe sind. Während die Wirtschaft wächst, steigt für jede_n einzelne_n von uns der Leistungsdruck und der Stress. Sinkt die Effizienz des „Rädchens“, wird es durch ein neues, produktiveres ersetzt."
        "exploitation_p1"       -> text "Alle Informationen, die über uns erhoben werden, können benutzt und verwertet werden. Das geschieht meistens, ohne dass wir uns wirklich darüber bewusst sind. Das Sammeln und Verkaufen von persönlichen Informationen ist eines der lukrativsten Geschäfte unserer Zeit."
        "exploitation_p2"       -> text "Personalisierte Werbung, generiert aus Informationen von Facebook und Google, und das Ausspähen deiner Emails durch den Staat sind dabei zwei Ausprägungen desselben Mechanismus."
        "money_p1"              -> text "Dein Lohn ist auch eine gesellschaftliche Währung für Anerkennung deiner Arbeit. Wer mehr verdient, macht die wichtigere, notwendigere und verantwortungsvollere Arbeit. Aber stimmt das wirklich? In der Zeit, die du im Museum warst, hat der Fußballspieler Philipp Lahm um einiges mehr verdient als ein_e Krankenpfleger_in. Ist das gerechtfertigt? Welcher Job ist wirklich wichtig für eine Gesellschaft? Und ist eine Gesellschaft, in der Anerkennung über Geld definiert wird, wünschenswert?"
        "thankyou"              -> text "Vielen Dank für den Besuch im Museum des Kapitalismus"
        "thankyou_p1"           -> text "Wir hoffen, dass dir die Ausstellung trotz Verlust von Geld und Zeit gefallen hat und dass du dafür etwas anderes mitnehmen konntest."
        "thankyou_p2"           -> text "Bitte vergiss nicht, deine Chipkarte zurückzugeben."
        "leave_subpage"         -> text "Zurück zur Auswahl"
        "next"                  -> text "Weiter"
        "previous"              -> text "Zurück"
        "football_t"            -> text "Fußballer Philip Lahm"
        "football_p"            -> text "wenn du Philip Lahm bist und für 830.000 EUR pro Monat Fußball spielst"
        "doctor_t"              -> text "Chefarzt_in"
        "doctor_p"              -> text "wenn du Chefarzt_in im Krankenhaus bist und 16.000 EUR pro Monat verdienst"
        "manager_t"             -> text "Manager_in"
        "manager_p"             -> text "wenn du Manager_in in einem Groß-konzern bist und 11.100 EUR pro Monat verdienst"
        "pilot_t"               -> text "Pilot_in"
        "pilot_p"               -> text "wenn du Pilot_in bist und 10.300 EUR pro Monat verdienst"
        "judge_t"               -> text "Richter_in"
        "judge_p"               -> text "wenn du Richter_in am Oberlandes-gericht bist und 6.100 EUR pro Monat verdienst"
        "teacher_t"             -> text "Lehrer_in"
        "teacher_p"             -> text "wenn du Lehrer_in bist und 4.030 EUR pro Monat verdienst"
        "mechanic_t"            -> text "Kfz-Mechaniker_in"
        "mechanic_p"            -> text "wenn du Kfz-Mechaniker_in bist und 2.830 EUR pro Monat verdienst"
        "salesman_t"            -> text "Versicherungs-kaufmann_frau"
        "salesman_p"            -> text "wenn du Versicherungs-kaufmann_frau bist und 2.576 EUR pro Monat verdienst"
        "nurse_t"               -> text "Kranken-pfleger_in"
        "nurse_p"               -> text "wenn du Krankenpfleger_in bist und 2.064 EUR pro Monat verdienst"
        "hairdresser_t"         -> text "Friseur_in"
        "hairdresser_p"         -> text "wenn du Friseur_in bist und 1.288 EUR pro Monat verdienst"
        "student_t"             -> text "Student_in"
        "student_p"             -> text "wenn du Student_in bist und 670 EUR BAföG bekommst"
        "sources"               -> text "Quellen: http://www.gehaltsreporter.de https://www.nettolohn.de http://www.lohnspiegel.de"
        _                       -> text ("Unknown. Key name: " ++ key)
    "en" ->
      case key of
        "welcome"               -> text "Welcome!"
        "place_chip"            -> text "Please place your chipcard on the reading device to receive your personal evaluation"
        "total_duration"        -> text "Duration of your visit at the Museum"
        "duration_hours"        -> text "hour"
        "duration_minutes"      -> text "minutes"
        "duration_by_area"      -> text "How long have you been in each subject area?"
        "needs"                 -> text "Needs and Exclusions"
        "mechanisms"            -> text "Mechanisms of Capitalism"
        "competition"           -> text "Competition and Crisis"
        "colonialism"           -> text "Colonialism"
        "beyond"                -> text "Beyond Capitalism"
        "compared_visit"        -> text "Your visit in comparison with the average visitor"
        "lost"                  -> text "You just lost time and money"
        "lost_explanation_p1"   -> text "In "
        "lost_explanation_p2"   -> text " minutes you could have earned a lot of money if you had worked instead of visiting the Museum"
        "point"                 -> text "What’s the point?"
        "point_explanation"     -> text "Touch the topics to find out more"
        "comparability"         -> text "Comparability and competition"
        "exploitation"          -> text "Exploitation of information"
        "money"                 -> text "Money and appreciation"
        "comparability_p1"      -> text "In capitalist societies we are supposed to compare us to others constantly. Competitiveness is measured in efficiency, competition is good for business – between companies as well as between colleagues. Do you stay longer in office as the others, are you always the first to show up? The technology to measure and compare all that exists since long ago. How long have you been at the museum? How efficient was your visit of the exhibition? And is time the right category for that?"
        "comparability_p2"      -> text "This exhibit is only one example for the fact, that human beings are only cogs in the big wheel. While the economy is growing, for each of us the pressure to perform and the stress rises. If the efficiency of a cog decreases, it is replaced by a new, more productive one."
        "exploitation_p1"       -> text "Every bit of information, that is collected of us, can be used and exploited. This happens mostly without us really being aware of it. The collecting and selling of personal informations is one of the most profitable businesses of our time."
        "exploitation_p2"       -> text "Personalized advertisement on the one hand, generated through informations from facebook and google and the spying on your emails by the state on the other hand are hereby two characteristics of the same mechanism."
        "money_p1"              -> text "Your salary is also a currency of society for appreciation of your work. Who earns more is doing the more important, more necessary and the more responsible work. But is this really true? In the time you spent in the museum, the football player Philipp Lahm earned a lot more than a nurse. Is that justified? Which job is really important for a society? And do we want to live in a society where appreciation is defined by money?"
        "thankyou"              -> text "Thank you for visiting the Museum of Capitalism"
        "thankyou_p1"           -> text "We hope that despite losing money you have enjoyed the exhibition and gained something from it."
        "thankyou_p2"           -> text "Please don’t forget to return your chip card!"
        "leave_subpage"         -> text "Back to menu"
        "next"                  -> text "Next"
        "previous"              -> text "Previous"
        "football_t"            -> text "Philip Lahm, football player"
        "football_p"            -> text "if you are Philip Lahm and play football for 830,000 EUR a month"
        "doctor_t"              -> text "chief physician"
        "doctor_p"              -> text "if you are a chief physician in a hospital making 16,000 EUR a month"
        "manager_t"             -> text "manager"
        "manager_p"             -> text "if you are a manager at a major corporation earning 11,100 EUR a month"
        "pilot_t"               -> text "pilot"
        "pilot_p"               -> text "if you are a pilot earning 10,300 EUR a month"
        "judge_t"               -> text "judge"
        "judge_p"               -> text "if you are a judge at a higher regional court earning 6,100 EUR a month"
        "teacher_t"             -> text "teacher"
        "teacher_p"             -> text "if you are a school teacher earning 4,030 EUR a month"
        "mechanic_t"            -> text "car mechanic"
        "mechanic_p"            -> text "if you are a car mechanic earning 2,830 EUR a month"
        "salesman_t"            -> text "insurance salesman"
        "salesman_p"            -> text "if you are an insurance salesman earning 2,576 EUR a month"
        "nurse_t"               -> text "nurse"
        "nurse_p"               -> text "if you are a nurse earning 2,064 EUR a month"
        "hairdresser_t"         -> text "hairdresser"
        "hairdresser_p"         -> text "if you are a hairdresser earning 1,288 EUR a month"
        "student_t"             -> text "student"
        "student_p"             -> text "if you are a student receiving 670 EUR BAföG"
        "sources"               -> text "Sources: http://www.gehaltsreporter.de https://www.nettolohn.de http://www.lohnspiegel.de"
        _                       -> text ("Unknown. Key name: " ++ key)



content: Address Action -> Model -> Html
content address model =
  case model.page of
    0 ->
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
        -- delete this button after RFID integration:
        ,
        button [ onClick address Next, class "overlay next" ]
        [
          text "Next"
        ]
      ]
    1 ->
      div [ class "back -second" ]
      [
        div [ class "main_item -in_two" ]
        [
          div [ class "content" ]
          [
            p [ class "title" ]
            [ phrase "welcome" "de" ]
            ,
            button [ onClick address (Language "de"), class "main_button" ]
            [
              p []
              [ text "Deutsch" ]
            ]
          ]
        ]
        ,
        div [ class "main_item -in_two" ]
        [
          div [ class "content" ]
          [
            p [ class "title" ]
            [ phrase "welcome" "en" ]
            ,
            button [ onClick address (Language "en"), class "main_button" ]
            [
              p []
              [ text "English" ]
            ]
          ]
        ]
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
              [ text "1" ]
              ,
              -- change phrase depending on singular or plural:
              phrase "duration_hours" model.language
            ]
            ,
            -- just show if minutes are not 0
            p [ class "lead"]
            [
              span [ class "big_number"]
              [ text "35" ]
              ,
              -- change phrase depending on singular or plural:
              phrase "duration_minutes" model.language
            ]
          ]
        ]
        ,
        -- if there are not data for next slide skip and go to slide 4
        button [ onClick address Next, class "overlay next"]
        [
          p []
          [ phrase "next" model.language ]
        ]
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
        button [ onClick address Previous, class "overlay previous"]
        [
          p []
          [ phrase "previous" model.language ]
        ]
        ,
        button [ onClick address Next, class "overlay next" ]
        [
          p []
          [ phrase "next" model.language ]
        ]
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
        button [ onClick address Previous, class "overlay previous"]
        [
          p []
          [ phrase "previous" model.language ]
        ]
        ,
        button [ onClick address Next, class "overlay next" ]
        [
          p []
          [ phrase "next" model.language ]
        ]
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
              [ text "9143€" ]
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
              [ text "176€" ]
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
              [ text "155€" ]
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
              [ text "123€" ]
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
              [ text "120€" ]
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
              [ text "111€" ]
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
              [ text "79€" ]
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
              [ text "73€" ]
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
              [ text "63€" ]
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
              [ text "22€" ]
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
              [ text "5€" ]
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
        button [ onClick address Previous, class "overlay previous"]
        [
          p []
          [ phrase "previous" model.language ]
        ]
        ,
        button [ onClick address Next, class "overlay next" ]
        [
          p []
          [ phrase "next" model.language ]
        ]
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
        button [ onClick address Previous, class "overlay previous"]
        [
          p []
          [ phrase "previous" model.language ]
        ]
        ,
        button [ onClick address Next, class "overlay next" ]
        [
          p []
          [ phrase "next" model.language ]
        ]
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
          ]
        ]
      ]

view : Address Action -> Model -> Html
view address model =
  content address model


-- MAIN

main : Signal Html
main =
  map (view actions.address) model
