port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode
import Set exposing (Set)
import Random.List
import Random
import Flip exposing (flip)
import Array
import List.Extra exposing (splitAt, cycle, getAt, groupsOfVarying)
--import Counter exposing (..)
--import Playground exposing (..)

port sendStuff : Json.Encode.Value -> Cmd msg

port receiveScrambled : (Json.Encode.Value -> msg) -> Sub msg
port receiveWordList : (Json.Encode.Value -> msg) -> Sub msg

---- MODEL ----

type Model
    = Loading GameOptions
    | Running GameState
    | GameOver GameState
    | Error

type alias Flags =
    { 
    }

type alias IndexedLetter =
    { index : Int
    , letter : String
    }

type alias GameOptions =
    { scrambledWords : List String
    , scrambledPhrase : String
    }

type alias GameState =
    { phrase : String
    , indexedMap : List (Int, String)
    , indexedWordMap : List (List (Int, String))
    , wordList : List String
    , numberOfWords : Int
    , spaceIndexes : List Int
    , letterIndexes : List Int
    , firstSpace : Int
    , secondSpace : Int
    , thirdSpace : Int
    , fourthSpace : Int
    , fifthSpace : Int
    , guesses : Set String
    , revealed : Int
    , words : List String
    , currentLevel : Int
    , wrongGuesses : List String
    , scrambledWords : List String
    , scrambledPhrase : String -- "I lvoe etanig cohlotae"
    , stillUnderscores : Bool
    , gameOver : Bool
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Loading { scrambledWords = [], scrambledPhrase = "" }
    , fetchSentence 1
    )


---- UPDATE ----


type Msg
    = Guess String
    | Restart
    | NewPhrase (Result Http.Error String)
    --| EndOfGame Bool --Bool being True if won, False if lost
    | SendData String
    | Received (Result Decode.Error String) 
    --| AssignSpaces (List Int)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> ( model, Cmd.none )

        Guess char ->
            case model of
                Running gameState ->
                    (  Running { gameState | guesses = Set.insert char gameState.guesses 
                               , wrongGuesses = List.filter (\char_ -> not <| String.contains char_ gameState.phrase) <| Set.toList <| Set.insert char gameState.guesses 
                               , stillUnderscores = seeIfBlanks <| List.map
                                                                                    (\char2 ->
                                                                                        if char2 == " " then
                                                                                            " "

                                                                                        else if Set.member char2 gameState.guesses then
                                                                                            char2

                                                                                        else
                                                                                            "_"
                                                                                    ) <| String.split "" <| gameState.phrase
                               }, sendStuff <| Json.Encode.int <| Set.size gameState.guesses )

                _ ->
                    ( model, Cmd.none )

        Restart ->
            ( Loading { scrambledWords = [], scrambledPhrase = "" }
            , fetchSentence 1)

        NewPhrase result ->
            case result of
                Ok phrase ->
                    case model of
                        Loading gameOptions ->
                            ( Running { phrase = phrase
                                    , words = String.split " " phrase
                                    , indexedMap = toIndexedMap phrase
                                    , indexedWordMap = []
                                    , wordList = toWordArray phrase
                                    , numberOfWords = List.length <| toWordArray phrase
                                    , spaceIndexes = getSpaceIndexes <| getSpaceElements <| toIndexedMap phrase
                                    , letterIndexes = [] --Calc
                                    , scrambledWords = []
                                    , firstSpace = sliceForFirst <| getSpaceIndexes <| getSpaceElements <| toIndexedMap phrase      
                                    , secondSpace = sliceForSecond <| getSpaceIndexes <| getSpaceElements <| toIndexedMap phrase
                                    , thirdSpace = sliceForThird <| getSpaceIndexes <| getSpaceElements <| toIndexedMap phrase
                                    , fourthSpace = 0
                                    , fifthSpace = 0
                                    , currentLevel = 1
                                    , scrambledPhrase = ""
                                    , guesses = Set.empty
                                    , revealed = 5
                                    , gameOver = False
                                    , wrongGuesses = [] 
                                    , stillUnderscores = True
                                    }, sendStuff <| Json.Encode.string phrase )
                        
                        Running gameState ->
                            ( Running { gameState | phrase = phrase
                                    , words = String.split " " phrase
                                    , scrambledWords = []
                                    , currentLevel = gameState.currentLevel + 1
                                    }, sendStuff <| Json.Encode.string phrase )
                        
                        _ ->
                            ( model, Cmd.none )

                Err _ ->
                    ( Error, Cmd.none )
{--
        EndOfGame bool ->
            case bool of
                True ->
                    ( GameOver True, Cmd.none )
                False ->
                    ( model, Cmd.none )
--}

        SendData string ->
            ( model, sendStuff <| Json.Encode.string string )
{--
        AssignSpaces list ->
            case model of
                Running gameState ->
                    case list.length of
                        2 ->
                            ( Running { gameState | firstSpace = list.head, secondSpace = list.tail }, Cmd.none )
                        3 ->
                            ( Running { gameState | firstSpace = list.head, secondSpace = List.Extra.getAt 1 list, thirdSpace = list.tail }, Cmd.none )
-}
        Received result ->
            case result of
                Ok value ->
                {-- Only if porting in from JS
                    case value of 
                        { first: _ } ->
                -}
                    case model of
                        Loading gameOptions ->
                            --( Loading { gameOptions | scrambledWords = value :: gameOptions.scrambledWords }, Cmd.none )
                            ( Loading { gameOptions | scrambledPhrase = value }, Cmd.none )

                        Running gameState ->
                            --( Running { gameState | scrambledWords = value :: gameState.scrambledWords }, Cmd.none )
                            case gameState.numberOfWords of
                             {-- 3 ->
                                    ( Running { gameState | scrambledPhrase = value.joinedWord }, Cmd.none )
                                4 ->
                                    ( Running { gameState | scrambledPhrase = value.joinedWord }, Cmd.none )
                                5 ->
                                    ( Running { gameState | scrambledPhrase = value.joinedWord }, Cmd.none )  -}  
                                _ ->
                                    ( Running { gameState | scrambledPhrase = value
                                              , indexedWordMap = getListOfWordGroups gameState --Need the spaces as ints first...not registering them yet  works with #s
                                              }, Cmd.none )                             
                            
                            --( Running { gameState | scrambledPhrase = value.joinedWord }, Cmd.none )
                
                        _ ->
                            ( model, Cmd.none )
        

                Err error ->
                    ( Error, Cmd.none )
                    --( { model | error = Decode.errorToString error }, Cmd.none )
{-
List.Extra.takeWhile (indexLessThanSpace spaceIndex) mapped

List.Extra.takeWhile (indexInRange gameState) indexMap

List.Extra.dropWhile (indexLessThanSpace spaceIndex) mapped
-}

sliceForFirst : List Int -> Int
sliceForFirst list =
    Array.fromList list
    |> Array.slice 0 1
    |> Array.toList
    |> listToInt

sliceForSecond : List Int -> Int
sliceForSecond list =
    Array.fromList list
    |> Array.slice 1 2
    |> Array.toList
    |> listToInt

sliceForThird : List Int -> Int
sliceForThird list =
    Array.fromList list
    |> Array.slice 2 3
    |> Array.toList
    |> listToInt

sliceForFourth : List Int -> Int
sliceForFourth list =
    Array.fromList list
    |> Array.slice 3 4
    |> Array.toList
    |> listToInt

listToInt : List Int -> Int
listToInt list =
    case list of
        [ a ] -> 
            a
        _ ->
            99

indexLessThanSpace : Int -> (Int, String) -> Bool
indexLessThanSpace space mappedLetter =
    if Tuple.first mappedLetter <= space then
        True
    else
        False

--Feed it a tuple of ints (starting index, ending index)
indexInRange : (Int, Int) -> (Int, String) -> Bool
indexInRange spaceTuple mappedLetter =
    if Tuple.first mappedLetter > Tuple.first spaceTuple then
        if Tuple.first mappedLetter <= Tuple.second spaceTuple then
            True
        else
            False
    else
        False


fetchSentence : Int -> Cmd Msg
fetchSentence int =
    Http.get
        { url = "https://api.hatchways.io/assessment/sentences/" ++ String.fromInt int
        , expect = Http.expectJson NewPhrase sentenceDecoder
        }

{--
updateGameState : GameState -> Cmd Msg
updateGameState gameState =
    get IndexedWordMap now
    Output the msg UpdateState
-}
{- Just port out.  Elm needs purity so I think that means always need to use a Generator type for randomness.
wordToScrambledWord : String -> String
wordToScrambledWord word =
    sendStuff word

    let
        firstLetterList = 
            String.split "" <| List.head <| String.split "" word
        lastLetterList = 
            String.split "" <| List.head <| List.reverse <| String.split "" word
    in
        String.split "" word
        |> List.drop 1
        |> List.reverse
        |> List.drop 1
        |> flip Random.step (Random.initialSeed 123)
        |> Tuple.first
        |> List.append firstLetterList
        |> flip List.append lastLetterList -- Here as a list of letter
-}

scramblePhrase : String -> Cmd Msg
scramblePhrase string =
    if String.length string <= 3 then
            sendStuff <| Json.Encode.string string
        else
            sendStuff <| Json.Encode.string string

getSpaceElements : List (Int, String) -> List (Int, String)
getSpaceElements list =
    List.filter isSpace list --this produces a List Int -- list of integers

-- THIS IS IT ==> groupsOfVarying : List Int -> List a -> List (List a)
--groupsOfVarying [ 2, 3, 1 ] [ "a", "b", "c", "d", "e", "f" ]
--> [ [ "a", "b" ], [ "c", "d", "e" ], [ "f" ] ]

getListOfWordGroups : GameState -> List (List (Int, String))
getListOfWordGroups gameState =
    case gameState.numberOfWords of
        3 -> 
            groupsOfVarying [ (gameState.firstSpace + 1)
                            , (gameState.secondSpace - gameState.firstSpace)
                            , (List.length gameState.indexedMap - gameState.secondSpace) 
                            ] gameState.indexedMap
        4 -> 
            groupsOfVarying [ (gameState.firstSpace + 1)
                            , (gameState.secondSpace - gameState.firstSpace)
                            , (gameState.thirdSpace - gameState.secondSpace)
                            , ((List.length gameState.indexedMap) - (gameState.thirdSpace + 1)) 
                            ] gameState.indexedMap
        5 -> 
            groupsOfVarying [ (gameState.firstSpace + 1)
                            , (gameState.secondSpace - gameState.firstSpace)
                            , (gameState.thirdSpace - gameState.secondSpace)
                            , (gameState.fourthSpace - gameState.thirdSpace)
                            , (List.length gameState.indexedMap - gameState.fourthSpace) 
                            ] gameState.indexedMap
        --No logic to the _.  Just need a default.
        _ ->
            groupsOfVarying [ (gameState.firstSpace + 1) ] gameState.indexedMap
{--
Could have also just used
elemIndices 1 [ 1, 2, 3 ]
--> [ 0 ]
so --
elemIndices " " letterList
or
elemIndices { _ : " " } indexedMap
for not just the index numbers, but the whole tuple
-}

isSpace : (Int, String) -> Bool
isSpace tuple =
    if Tuple.second tuple == " " then
        True
    else
        False

getSpaceIndexes : List (Int, String) -> List Int
getSpaceIndexes list =
    List.map extractInt list

extractInt : (Int, String) -> Int
extractInt tuple =
    Tuple.first tuple
{--
breakUpIndexedMap : List (Int, String) -> List Int -> List List (Int, String)
breakUpIndexedMap indexedMap spaceIndexList =
    let
        firstElementFollowingSpace = 1 + List.head spaceIndexList
    in
        List.cycle firstElementFollowingSpace indexedMap

forEachSpace : List Int -> List (Int, String)
forEachSpace spaceIndexList =
    List.map getSplitIndexedMap spaceIndexList

getSplitIndexedMap : Int -> List (Int, String)
getSplitIndexedMap int =
-}

sentenceDecoder : Decoder String
sentenceDecoder =
    Decode.at [ "data", "sentence" ] Decode.string
{--
sentenceDecoder : Decoder String
sentenceDecoder =
    Decode.at [ "value", "wordIndexObj" ] Decode.string
-}
scrambledWordDecoder : Decoder String
scrambledWordDecoder =
    Decode.at [ "value", "joinedWord" ] Decode.string

indexDecoder : Decoder String
indexDecoder =
    Decode.index 0 Decode.string

intDecoder : Decoder Int
intDecoder =
    Decode.field "numberOfWords" Decode.int

valueStrDecoder : Decoder String
valueStrDecoder =
    Decode.field "value" Decode.string
--Gotta decode for each word - shit

iSubMapDecoder : Decoder (List IndexedLetter)
iSubMapDecoder =
  Decode.list indexedListDecoder

indexedListDecoder : Decoder IndexedLetter
indexedListDecoder =
  Decode.map2
    IndexedLetter
    (Decode.at [ "indexObj", "index" ] Decode.int)
    (Decode.at [ "indexObj", "letter" ] Decode.string)


boolDecoder : Decoder Bool
boolDecoder =
    Decode.field "gameOver" Decode.bool

subscriptions : Model -> Sub Msg
subscriptions model =
    receiveScrambled (Decode.decodeValue valueStrDecoder >> Received)

---- VIEW ----


view : Model -> Html Msg
view model =
    case model of 
        Loading gameOptions ->
            div [] [ Html.text "Loading" ]

        Running gameState ->
            case List.length gameState.wrongGuesses of
                7 ->
                    viewGameOver gameState
                _ ->
                    case gameState.stillUnderscores of
                        True ->
                            viewGameState gameState
                        False ->
                            viewGameOver gameState


        GameOver gameState ->
            viewGameOver gameState

        Error ->
            div [] [ Html.text "Error" ]

seeIfBlanks : List String -> Bool
seeIfBlanks list =
    List.member "_" list
{--
winWhenZero : Bool -> Int
winWhenZero bool =
        case bool of
            False ->
                div [ Html.Attributes.class "gameover" ] [ Html.text "Congrats! You have stumped the monster! Would you like to try again and continue defending the world?"]
            True ->
                div [ Html.Attributes.class "gameover" ] [] 
--}
toIndexedMap : String -> List (Int, String)
toIndexedMap string =
    String.split "" string
    |> List.indexedMap Tuple.pair

toWordArray : String -> List String
toWordArray string =
    String.split " " string

winWhenZeroHtml : Bool -> Html Msg
winWhenZeroHtml bool =
        case bool of
            False ->
                div [ Html.Attributes.class "gameover" ] [ Html.text "Congrats! You have stumped the monster! Guess any letter once more to see him destroyed."]
            True ->
                div [ Html.Attributes.class "gameover" ] [] 


viewInt : Int -> Html Msg
viewInt count =
    div []
        [ div [] [ Html.text (String.fromInt count) ]
        ]
--Int should be gameState.revealed
checkIfRevealed : (Int, String) -> Int -> String
checkIfRevealed tuple revealed =
    if revealed > Tuple.first tuple then
        Tuple.second tuple
    else
        " "

viewLetter : Int -> (Int, String) -> Html msg
viewLetter revealed indexedLetter =
    div [ Html.Attributes.class "grid-item" ] 
    [ li [ Html.Attributes.class "listLetter" ] [ Html.text (checkIfRevealed indexedLetter revealed ) ] ]
{--
viewRow : List (Int, String) -> Html msg
viewRow indexedLetter =
    div [ Html.Attributes.class "listLetterBox" ] 
    [ li [ Html.Attributes.class "listLetter" ] [ Html.text (Tuple.second indexedLetter) ] ]
-}
    --div [] [ checkIfRevealed indexedLetter ]
    --Show either nothing or the letter

viewKeyboard : Int -> List (List (Int, String)) -> List( List (Html msg))
viewKeyboard int indexedWordMap =
    List.map (outputHtmlRow int) indexedWordMap

outputHtmlRow : Int -> List (Int, String) -> List (Html msg)
outputHtmlRow int rowWord =
    List.map (viewLetter int) rowWord

extractHtmlList : List ( List(Html msg)) -> List (Html msg)
extractHtmlList htmlList =
    List.map getInnerList htmlList

getInnerList : List(Html msg) -> Html msg
getInnerList outerList =
    outerList
    |> createInnerList
    |> Html.ul [] 

createInnerList : List(Html msg) -> List(Html msg)
createInnerList outerList =
    List.map addBrTo outerList

addBrTo : Html msg -> Html msg
addBrTo html =
    div [ Html.Attributes.class "grid-container grid-container--fit listLetterBox" ] [ html
           , br [] []
           ]

viewGameOver : GameState -> Html Msg
viewGameOver gameState =
    case gameState.stillUnderscores of
        True ->
            div [] [ 
                div [] [ h1 [] [ Html.text "Game Over. You have failed your fellow man. The Monster will keep growing." ]
                       , br [] []
                       , img [src "./fpm.png", Html.Attributes.class "losingEnd" ] [] 
                       , br [] []
                       , button [ onClick Restart ] [ Html.text "Play Again" ] ]
                       ]
        False ->
            div [] [ 
                div [] [ h1 [] [ Html.text "You did it! You have saved the world from this awful Monster! Be gone with him!" ]
                       , img [ src "./fpm.png", Html.Attributes.class "winningEnd" ] []
                       , br [] [] 
                       , button [ onClick Restart ] [ Html.text "Play Again" ] ]
                       ]
                       
viewGameState : GameState -> Html Msg
viewGameState gameState =
    let
        phraseHtml =
            gameState.indexedMap
                |> List.map --Have this as a list of { index: 0, letter: "a" }
                    (\tuple ->
                        span [ Html.Attributes.class "keyLetter" ] [ Html.text <| checkIfRevealed tuple gameState.revealed ]
                    )
                |> div [ Html.Attributes.id "keyButton" ]

        phraseSet =
            gameState.phrase
            |> String.split ""
            |> Set.fromList

        failuresHtml =
            gameState.guesses
            |> Set.toList
            |> List.filter (\char -> not <| Set.member char phraseSet)
            |> List.map (\char -> span [] [ Html.text char ])
            |> div [ Html.Attributes.class "failures" ]

        hangmanHtml =
            gameState.guesses
            |> Set.toList
            |> List.filter (\char -> not <| Set.member char phraseSet)
            |> List.length
            |> viewInt

        gameOverHtml =
            gameState.phrase
                |> String.split ""
                |> List.map
                    (\char ->
                        if char == " " then
                            " "

                        else if Set.member char gameState.guesses then
                            char

                        else
                            "_"
                    )
            |> seeIfBlanks
            |> winWhenZeroHtml

        keyboardHtml =
            gameState.wordList
            |> List.map (\word ->
                --span [ Html.Attributes.class "keyLetter" ] [ Html.text <| checkIfRevealed char <| getIndex gameState ]
                div [ Html.Attributes.id "keyboardWord" ] [ wordHtml ]
                )
            |> div [ Html.Attributes.id "wholeKeyboard" ]

        pleaseHtml =
            gameState.indexedWordMap
            |> viewKeyboard gameState.revealed
            |> extractHtmlList
            |> getInnerList

        wordHtml =
            gameState.indexedMap
            |> List.map (\tuple ->
                --span [ Html.Attributes.class "keyLetter" ] [ Html.text <| checkIfRevealed char <| getIndex gameState ]
                span [ Html.Attributes.class "keyLetter" ] [ Html.text <| checkIfRevealed tuple gameState.revealed ]
                )
            |> div [ Html.Attributes.class "letterBtns" ]

    in
    div []
        [ h1 [] [ Html.text "Guess The Phrase" ]
        , h3 [ Html.Attributes.class "" ] [ Html.text "Cant you correctly unscramble the phrase?" ]
        , h1 [ Html.Attributes.class "scrambledPhrase" ] [ Html.text gameState.scrambledPhrase ]
        , br [] []
        , h4 [] [ Html.text "Keyboard" ]
        --, phraseHtml
        , br [] []
        , br [] []
        --, wordHtml

        , pleaseHtml
        , gameOverHtml
        , div [ Html.Attributes.id "resignArea" ] [ h3 [] [ Html.text "Feel Free to Start Over if You Can't Handle it"]
                                                  , button [ onClick Restart, Html.Attributes.class "btnClass" ] [ Html.text "Resign" ]
                                                  ]
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
