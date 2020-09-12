module Main exposing (main)

import Browser
import Html exposing (Html, text, pre)
import Http
import Json.Decode as JD exposing (Decoder, field, int, string, list)
import List exposing (head)


-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type Model
  = Failure String
  | Loading
  | Success (List Question)


type alias Question =
    { 
      -- category : String,
    -- difficulty: String,
    question: String
    -- correct_answer: String,
    -- incorrect_answers: List String
    }

type alias Questions = List Question



init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , fetchQuestion
  )





-- UPDATE


type Msg
  = GotText (Result Http.Error (List Question))





update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok data ->
          (Success data, Cmd.none)

        Err err ->
          (Failure (Debug.toString(err)), Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  case model of
    Failure msg ->
      text msg

    Loading ->
      text "Loading..."

    Success questions ->
      pre [] [ text (Maybe.withDefault {question= ""} (head questions) ).question ]



-- HTTP

fetchQuestion : Cmd Msg
fetchQuestion = Http.get
      { url = "https://opentdb.com/api.php?amount=10&type=multiple"
      , expect = Http.expectJson GotText questionsDecoder
      }

questionDecoder: Decoder Question
questionDecoder = 
  JD.map Question
    (field "question" string)

questionsDecoder: Decoder (List Question)
questionsDecoder = 
  field "results" (JD.list questionDecoder) 