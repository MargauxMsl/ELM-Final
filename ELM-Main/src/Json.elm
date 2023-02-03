-- Press a button to send a GET request for random wordInfos.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/json.html
--
module Json exposing (..)

import Browser
import Array
import String
import Random
import Html exposing (..)
import Html exposing (Html, Attribute, button, div, form, h1, input, text)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Html.Events exposing (onInput, onClick)
import Http
import Json.Decode exposing (Decoder, map4, field, int, string)


type alias Model =
    { state : State
    , listOfWords : List String
    , index : Int
    , word : String
    , wordInfo : List WordInfo
    , guess : String
    }

type State
    = Failure String
    | Loading
    | Success
    | Sleep
    | GotList
    | GotNumber
    | GotDefinitions
    | WinOrLose

type alias WordInfo =
    { word : String
    , meanings : List Meaning
    }

type alias Meaning =
    { partOfSpeech : String
    , definitions : List Definition
    }

type alias Definition = 
    { definition : String
    }

type Msg
    = GotText (Result Http.Error String)
    | GotIndex Int
    | GotWord String
    | GotWordInfo (Result Http.Error (List WordInfo))
    | NewGuess String
    | CheckAnswer
    | LoadingModel


---------- MAIN ------------------------------------

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

--------- INIT --------------------------------------

init : () -> (Model, Cmd Msg)
init _ =
    (Model Loading [] 0 "" [] "" ,
    Http.get
        {url = "./Data.txt"
        , expect= Http.expectString GotText})

--------- UPDATE ------------------------------------

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok string ->
                    ({ model | listOfWords = String.words string, state = GotList }
                    , getIndex model)
                
                Err _ ->
                    ({model | state = Failure "error in get text"}
                    , Cmd.none)
        
        GotIndex int ->
          case (getWord model.listOfWords int) of
            Just string ->
              ({ model | index = int, word = string, state = GotNumber}
              , Http.get
                { url = ("https://api.dictionaryapi.dev/api/v2/entries/en/" ++ string)
                , expect = Http.expectJson GotWordInfo listDecoder
                })
            Nothing -> 
              (model, Cmd.none)

        NewGuess guess ->
            ({model| guess = guess}
            , Cmd.none
            )
        
        CheckAnswer ->
            if (String.toLower model.guess) == (String.toLower model.word) then
                ({model | state = WinOrLose}
                , Cmd.none
                )
            else
                ({model | state = WinOrLose}
                , Cmd.none
                )
        
       -- GotWord word ->
        
        GotWordInfo result ->
            case result of
                Ok def ->
                    ({model | state = GotDefinitions, wordInfo=def }
                    , Cmd.none)
                
                Err _ ->
                    ({model | state = Failure "error in get json"} , Cmd.none)
        
        LoadingModel->
            ({model | guess=""} 
            , Http.get {url = "./Data.txt", expect= Http.expectString GotText})
        _ ->
            (model, Cmd.none)

------------- SUBSCRIPTIONS ----------------------------

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.none
------- VIEW ------------------------------------------

view : Model -> Html Msg
view model =
    case model.state of

        GotDefinitions ->
          div backFormStyle
                    [ h1 headerStyle [ text "Guess the W _ _ _ ! "] 
                    , div textStyle
                        [div [] (List.map viewWordInfo model.wordInfo)]
                    , div answerFieldStyle
                        [ input [Html.Attributes.placeholder "Who am I ?", Html.Attributes.value model.guess, onInput NewGuess] []
                        , button [onClick CheckAnswer] [text "Submit"]
                        ]
                    ]         

        GotList ->
            div [] (List.map text model.listOfWords)
        
        GotNumber ->
            text ((String.fromInt model.index) ++ model.word)
        
        Failure error ->
            text error
        
        WinOrLose ->
            div []
                    [ checkAnswer model ]
        
        _ ->
          text "we will see that later"


viewWordInfo : WordInfo -> Html Msg
viewWordInfo wordInfo =
  div []
    [ p textStyle (List.map viewMeaning wordInfo.meanings)
    ]

viewMeaning : Meaning -> Html Msg
viewMeaning dictionary = 
    div []
        [ li partOfSpeechStyle [text dictionary.partOfSpeech]
        , ul [style "margin-top" "15px"] (List.map viewDefinition dictionary.definitions)
        ]

viewDefinition : Definition -> Html Msg
viewDefinition def =
    div [] 
        [ ul [] [text def.definition]] 

checkAnswer : Model -> Html Msg
checkAnswer model = 
    if model.guess == model.word then
        div rightAnswerStyle
            [text "Congratulations ! \n Wanna play again ?"
            , div []
                [ button ([onClick LoadingModel] ++ ([Html.Attributes.type_ "Yes"]  ++ yesButtonStyle))
                    [text "Yes"]
                ]
            ]

    else if model.guess=="" then
        view model

    else
        div wrongAnswerStyle
            [ p [style "margin-top" "20px", style "margin-bottom" "10px"] [ text "Wrong answer ! \n Wanna try again ?"]
            , div [] 
                [ button ([onClick (GotIndex model.index)] ++ ([Html.Attributes.type_ "Yes"]  ++ yesButtonStyle))
                    [text "Yes"]
                ,  button ([onClick LoadingModel] ++ ([Html.Attributes.type_ "No"] ++ noButtonStyle))
                    [text "new word"]
                ]
            ]
----------------- JSON --------------------------------------------

listDecoder : Decoder (List WordInfo)
listDecoder =
    Json.Decode.list wordDecoder
  
wordDecoder : Decoder WordInfo
wordDecoder = 
    Json.Decode.map2 WordInfo
        (field "word" string)
        (field "meanings" <| Json.Decode.list meaningDecoder)

meaningDecoder : Decoder Meaning
meaningDecoder =
    Json.Decode.map2 Meaning
        (field "partOfSpeech" string)
        (Json.Decode.field "definitions" <| Json.Decode.list definitionDecoder)

definitionDecoder : Decoder Definition
definitionDecoder = 
    Json.Decode.map Definition
        (Json.Decode.field "definition" Json.Decode.string)

------ HELPERS ----------------------------------------

getIndex : Model -> Cmd Msg
getIndex model =
    Random.generate GotIndex (Random.int 0 999)

getWord : List a -> Int -> Maybe a
getWord list int = List.head (List.drop int list)

headerStyle : List (Attribute msg)
headerStyle = 
    [ style "text-align" "center"
    , style "margin-top" "50px"
    , style "fontSize" "80px"
    , style "fontWeight" "Light"
    , style "color" "white"
    , style "font-family" "Bodoni MT Condensed"
    ]

backFormStyle : List (Attribute msg)
backFormStyle =
    [ style "border-radius" "5px"
    , style "background-color" "rgb(242, 104, 36)"
    , style "text-align" "center"
    , style "width" "fill"
    , style "margin" "auto"
    , style "margin-top" "15px"
    , style "position" "absolute"
    , style "top" "50%"
    , style "left" "50%"
    , style "transform" "translate(-50%, -50%)"
    ]

formStyle : List (Attribute msg)
formStyle =
    [ style "border-radius" "5px"
    , style "background-color" "rgb(239, 187, 167)"
    , style "text-align" "center"
    , style "width" "300px"
    , style "margin" "auto"
    , style "margin-top" "15px"
    , style "position" "absolute"
    --, style "top" "50%"
    , style "left" "50%"
    , style "transform" "translate(-50%, -50%)"
    ]

rightAnswerStyle : List (Attribute msg)
rightAnswerStyle = [ style "border-radius" "5px"
    , style "background-color" "rgb(162, 217, 152)"
    , style "text-align" "center"
    , style "width" "300px"
    , style "margin" "auto"
    , style "position" "absolute"
    , style "top" "50%"
    , style "left" "50%"
    , style "transform" "translate(-50%, -50%)"
    ]

wrongAnswerStyle : List (Attribute msg)
wrongAnswerStyle = [ style "border-radius" "5px"
    , style "background-color" "rgb(162, 217, 152)"
    , style "text-align" "center"
    , style "width" "300px"
    , style "margin" "auto"
    , style "position" "absolute"
    , style "top" "50%"
    , style "left" "50%"
    , style "transform" "translate(-50%, -50%)"
    ]


yesButtonStyle : List (Attribute msg)
yesButtonStyle = 
    [ style "width" "100px"
    , style "height" "40px"
    , style "border-radius" "15"
    , style "background-color" "#397cd5"
    , style "color" "white"
    , style "padding" "14px 20px"
    , style "margin-top" "10px"
    , style "margin-right" "15px"
    , style "margin-bottom" "10px"
    , style "border" "none"
    , style "border-radius" "4px"
    , style "font-size" "11px"
    ]

noButtonStyle : List (Attribute msg)
noButtonStyle = 
    [ style "width" "100px"
    , style "height" "40px"
    , style "border-radius" "15"
    , style "background-color" "red"
    , style "color" "white"
    , style "padding" "14px 20px"
    , style "margin-top" "10px"
    , style "margin-left" "15px"
    , style "margin-bottom" "10px"
    , style "border" "2px"
    , style "border-radius" "4px"
    , style "font-size" "11px"
    ]
  
textStyle : List (Attribute msg)
textStyle =
    [style "text-align" "justified"
    , style "text-align" "left"
    , style "margin-left" "30px"
    , style "margin-right" "30px"
    ]

partOfSpeechStyle : List (Attribute msg)
partOfSpeechStyle =
    [ style "fontweight" "bold"
    , style "texte-align" "center"
    , style "font-size" "30px"
    , style "color" "white"
    , style "margin-top" "15px"
    ]

answerFieldStyle : List (Attribute msg)
answerFieldStyle =
    [ style "text-align" "center"
    , style "margin-bottom" "30px"
    ]