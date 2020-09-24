module Main exposing (main)

import Browser
import Html exposing (Html, button, div, pre, text)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD exposing (Decoder, bool, field, list, map6, string)
import List exposing (append, map, sort)
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
    { category : String
    , difficulty : String
    , question : String
    , correct_answer : String
    , incorrect_answers : List String
    , correct : Maybe Bool
    , selected_answer : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , fetchQuestions
    )



-- UPDATE


type Msg
    = GotText (Result Http.Error (List Question))
    | SetAnswer (List Question) Question String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok data ->
                    ( Success data, Cmd.none )

                Err err ->
                    ( Failure (Debug.toString err), Cmd.none )

        SetAnswer questions question answer ->
            ( Success (setCorrect questions question answer), Cmd.none )


setCorrect : List Question -> Question -> String -> List Question
setCorrect questions question answer =
    map
        (\q ->
            if q.question == question.question && q.correct == Nothing then
                { question | correct = Just (question.correct_answer == answer), selected_answer = Just answer }

            else
                q
        )
        questions



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
            div [ style "text-align" "center" ]
                [ div [] [ createHTMLElementsForQuestions questions ] ]



-- HTTP


fetchQuestions : Cmd Msg
fetchQuestions =
    Http.get
        { url = "https://opentdb.com/api.php?amount=10&type=multiple"
        , expect = Http.expectJson GotText questionsDecoder
        }


createHTMLElementsForQuestions : List Question -> Html Msg
createHTMLElementsForQuestions questions =
    div []
        (map
            (\question ->
                div [ style "margin" "2rem", style "padding" "2rem", style "padding-top" "0rem" ]
                    [ div [] [ text question.question ]
                    , div [ style "margin-top" "1rem" ]
                        (map
                            (\answer ->
                                button
                                    [ onClick (SetAnswer questions question answer)
                                    , style "margin" "0.3rem "
                                    , style "padding" "0.4rem"
                                    , case question.selected_answer of
                                        Nothing ->
                                            style "color" "black"

                                        Just selectedAnswer ->
                                            if selectedAnswer == answer then
                                                style "color" "blue"

                                            else
                                                style "color" "gray"
                                    , case question.selected_answer of
                                        Nothing ->
                                            disabled False

                                        Just selectedAnswer ->
                                            disabled True
                                    ]
                                    [ text answer ]
                            )
                            (sort (question.correct_answer :: question.incorrect_answers))
                        )
                    , div [ style "margin-top" "1rem" ] [ displayResult question ]
                    ]
            )
            questions
        )


displayResult : Question -> Html Msg
displayResult question =
    case question.correct of
        Nothing ->
            div [] []

        Just isCorrect ->
            if isCorrect then
                div [ style "color" "#0F9D58" ] [ text "Correct Answer!" ]

            else if not isCorrect then
                div [ style "color" "#db4437" ] [ text "Wrong! ", text ("Correct Answer: " ++ question.correct_answer) ]

            else
                div [] []


questionDecoder : Decoder Question
questionDecoder =
    JD.map7 Question
        (field "category" string)
        (field "difficulty" string)
        (field "question" string)
        (field "correct_answer" string)
        (field "incorrect_answers" (JD.list string))
        (JD.succeed Nothing)
        (JD.succeed Nothing)


questionsDecoder : Decoder (List Question)
questionsDecoder =
    field "results" (JD.list questionDecoder)
