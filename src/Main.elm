module Main exposing (main)

import Browser
import Html exposing (Html, text, div, pre, button)
import Html.Attributes exposing (style)
import Http
import Json.Decode as JD exposing (Decoder, field, string, list, map6)
import List exposing (map, append)
import String exposing (join)


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
      category : String,
    difficulty: String,
    question: String,
    correct_answer: String,
    incorrect_answers: List String
    }


init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , fetchQuestions
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
      div [style "text-align" "center", style "margin-top" "10rem"] 
      [ div [] [createHTMLElementsForQuestions questions] ]



-- HTTP

fetchQuestions : Cmd Msg
fetchQuestions = Http.get
      { url = "https://opentdb.com/api.php?amount=10&type=multiple"
      , expect = Http.expectJson GotText questionsDecoder
      }

createHTMLElementsForQuestions: (List Question) -> Html msg
createHTMLElementsForQuestions questions = 
  div [] (map (\x -> 
  div[style "margin" "2rem", style "padding" "2rem" ] [ 
    div [] [ text x.question ], 
    div [] ((button [style "margin" "0.3rem  "] [text x.correct_answer]) :: (map (\y -> button [style "margin" "0.3rem  "] [text y]) x.incorrect_answers))
  ]
   ) questions)


questionDecoder: Decoder Question
questionDecoder = 
  JD.map5 Question
    (field "category" string)
    (field "difficulty" string)
    (field "question" string)
    (field "correct_answer" string)
    (field "incorrect_answers" (JD.list string))

 
questionsDecoder: Decoder (List Question)
questionsDecoder = 
  field "results" (JD.list questionDecoder) 