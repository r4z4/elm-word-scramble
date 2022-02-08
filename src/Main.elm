port module Main exposing (..)

import Browser
import Browser.Events
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
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
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

type Key
  = Character Char
  | Control String

type KeyEventMsg
    = KeyEventControl
    | KeyEventLetter Char
    {--
    | KeyEventUnknown String
    | KeyEventAlt
    | KeyEventShift
    | KeyEventMeta
    -}


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
    , keyPressed : String
    , letterIndexes : List Int
    , firstSpace : Int
    , secondSpace : Int
    , thirdSpace : Int
    , fourthSpace : Int
    , fifthSpace : Int
    , indexedWordOne : List (Int, String)
    , indexedWordTwo : List (Int, String)
    , indexedWordThree : List (Int, String)
    , indexedWordFour : List (Int, String)
    , indexedWordFive : List (Int, String)
    , displayMsg : String
    , revealed : Int
    , words : List String
    , currentLevel : Int
    , currentLetterIndex : Int
    , currentLetterString : String
    , wrongGuesses : Int
    , nextButton : Bool
    , gameOver : Bool
    , errorMsg : String
    , scrambledWords : List String
    , scrambledPhrase : String -- "I lvoe etanig cohlotae"
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Loading { scrambledWords = [], scrambledPhrase = "" }
    , fetchSentence 1
    )


---- UPDATE ----


type Msg
    = Restart
    | NewPhrase (Result Http.Error String)
    --| EndOfGame Bool --Bool being True if won, False if lost
    | SendData String
    | Received (Result Decode.Error String) 
    | NextRound Int
    | KeyPressed Key
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> ( model, Cmd.none )

        NextRound int ->
            case model of
                Running gameState ->
                    case int of
                        -- Figure out which number to match
                        -- No effect of this pattern match.  It never gets to 11 because the fetchSentence fires before and we cannot fetch an /11 url
                        11 ->
                            ( Running { gameState | gameOver = True }, Cmd.none )

                        _ ->
                            ( Running { gameState | currentLevel = int }, fetchSentence int )
                _ ->
                    ( model, Cmd.none )

--Case Sensative as of now
        KeyPressed key ->
            case model of
                GameOver gameState ->
                    case key of
                        Control "Enter" ->
                            ( Running { gameState | currentLevel = 1 }, fetchSentence 1 )
                        _ ->
                            ( GameOver { gameState | errorMsg = "Please press Enter to continue." }, Cmd.none )

                Running gameState ->
                    if gameState.revealed == List.length gameState.indexedMap then
                        case key of
                            Control "Enter" ->
                                case gameState.currentLevel of
                                    10 ->
                                        ( GameOver { gameState | displayMsg = "You Win." }, Cmd.none ) 
                                    _ ->
                                        ( Running { gameState | currentLevel = gameState.currentLevel + 1 }, fetchSentence (gameState.currentLevel + 1) )
                            
                            _ ->
                                ( Running { gameState | errorMsg = "Please press Enter to continue." }, Cmd.none )
                    else
                        case key of
                            Character char ->
                                if String.fromChar char == gameState.currentLetterString then
                                    ( Running { gameState | keyPressed = String.fromChar char
                                            , currentLetterIndex = gameState.currentLetterIndex + 1
                                            , revealed = gameState.revealed + 1
                                            , nextButton = ( gameState.revealed == (List.length gameState.indexedMap - 1) )
                                            , currentLetterString = extractString <| Maybe.withDefault (99, "Defaults") <| List.Extra.getAt (gameState.revealed + 1) <| gameState.indexedMap
                                            }, Cmd.none )
                                else
                                    ( Running { gameState | keyPressed = String.fromChar char
                                            , wrongGuesses = gameState.wrongGuesses + 1
                                            }, Cmd.none )

                            _ ->
                                ( model, Cmd.none )

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
                                    , keyPressed = "" --Representing here as a String, but comes in as Char
                                    , scrambledWords = []
                                    , firstSpace = sliceForFirst <| getSpaceIndexes <| getSpaceElements <| toIndexedMap phrase      
                                    , secondSpace = sliceForSecond <| getSpaceIndexes <| getSpaceElements <| toIndexedMap phrase
                                    , thirdSpace = sliceForThird <| getSpaceIndexes <| getSpaceElements <| toIndexedMap phrase
                                    , fourthSpace = Maybe.withDefault 99 <| List.Extra.getAt 3 <| getSpaceIndexes <| getSpaceElements <| toIndexedMap phrase
                                    , fifthSpace = Maybe.withDefault 99 <| List.Extra.getAt 4 <| getSpaceIndexes <| getSpaceElements <| toIndexedMap phrase
                                    , indexedWordOne = []
                                    , indexedWordTwo = []
                                    , indexedWordThree = []
                                    , indexedWordFour = []
                                    , indexedWordFive = []
                                    , currentLevel = 1
                                    , currentLetterIndex = 0
                                    , currentLetterString = extractString <| Maybe.withDefault (99, "Defaults") <| List.head <| toIndexedMap phrase
                                    --Itll be the same ^^^ but just sub List.head func with the getAt func 
                                    , scrambledPhrase = ""
                                    , errorMsg = ""
                                    , displayMsg = ""
                                    , revealed = 0
                                    , gameOver = False
                                    , nextButton = False
                                    , wrongGuesses = 0
                                    }, sendStuff <| Json.Encode.string phrase )
                        
                        Running gameState ->
                            ( Running { phrase = phrase
                                    , words = String.split " " phrase
                                    , indexedMap = toIndexedMap phrase
                                    , indexedWordMap = []
                                    , wordList = toWordArray phrase
                                    , numberOfWords = List.length <| toWordArray phrase
                                    , spaceIndexes = getSpaceIndexes <| getSpaceElements <| toIndexedMap phrase
                                    , letterIndexes = [] --Calc
                                    , keyPressed = "" --Representing here as a String, but comes in as Char
                                    , scrambledWords = []
                                    , firstSpace = sliceForFirst <| getSpaceIndexes <| getSpaceElements <| toIndexedMap phrase      
                                    , secondSpace = sliceForSecond <| getSpaceIndexes <| getSpaceElements <| toIndexedMap phrase
                                    , thirdSpace = sliceForThird <| getSpaceIndexes <| getSpaceElements <| toIndexedMap phrase
                                    , fourthSpace = Maybe.withDefault 99 <| List.Extra.getAt 3 <| getSpaceIndexes <| getSpaceElements <| toIndexedMap phrase
                                    , fifthSpace = Maybe.withDefault 99 <| List.Extra.getAt 4 <| getSpaceIndexes <| getSpaceElements <| toIndexedMap phrase
                                    , indexedWordOne = []
                                    , indexedWordTwo = []
                                    , indexedWordThree = []
                                    , indexedWordFour = []
                                    , indexedWordFive = []
                                    , currentLevel = gameState.currentLevel
                                    , currentLetterIndex = 0
                                    , currentLetterString = extractString <| Maybe.withDefault (99, "Defaults") <| List.head <| toIndexedMap phrase
                                    --Itll be the same ^^^ but just sub List.head func with the getAt func 
                                    , scrambledPhrase = ""
                                    , revealed = 0
                                    , errorMsg = ""
                                    , displayMsg = ""
                                    , gameOver = False
                                    , nextButton = False
                                    , wrongGuesses = 0
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
                                ( Running { gameState | scrambledPhrase = value
                                          , indexedWordMap = getListOfWordGroups gameState --Need the spaces as ints first...not registering them yet  works with #s
                                          , indexedWordOne = Maybe.withDefault [] <| List.Extra.getAt 0 <| getListOfWordGroups gameState
                                          , indexedWordTwo = Maybe.withDefault [] <| List.Extra.getAt 1 <| getListOfWordGroups gameState
                                          , indexedWordThree = Maybe.withDefault [] <| List.Extra.getAt 2 <| getListOfWordGroups gameState
                                          , indexedWordFour = Maybe.withDefault [] <| List.Extra.getAt 3 <| getListOfWordGroups gameState
                                          , indexedWordFive = Maybe.withDefault [] <| List.Extra.getAt 4 <| getListOfWordGroups gameState
                                          }, Cmd.none )                                         
                        _ ->
                            ( model, Cmd.none )
        

                Err error ->
                    ( Error, Cmd.none )
                    --( { model | error = Decode.errorToString error }, Cmd.none )

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

sliceForFifth : List Int -> Int
sliceForFifth list =
    Array.fromList list
    |> Array.slice 4 5
    |> Array.toList
    |> listToInt
{-
checkIfComplete : GameState -> Msg
checkIfComplete gameState =
    if gameState.revealed + 1 == (List.length gameState.indexedMap) then
        NextRound (gameState.currentLevel + 1)
    else
        Cmd.none
-}
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
{--
getCurrentLetter : Int -> List (Int, String) -> String
getCurrentLetter int indexedMap =
-}
getMaybeCurrentLetter : Int -> List (Int, String) -> (Int, String)
getMaybeCurrentLetter int indexedMap =
    Maybe.withDefault (99, "Defaults") (List.Extra.getAt int indexedMap)

extractString : (Int, String) -> String
extractString tuple =
    Tuple.second tuple

{--Unwrapping a Maybe
commandIf : Maybe (Int, String) -> String
commandIf (int, string) =
  case (int, string) of
    (Just int_, Just string_) ->
        string

    _ ->
        "blouse"

createGuessTuple : Int -> String -> (Int, String)
createGuessTuple int string =
    (int, string)
--}

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
        2 -> 
            groupsOfVarying [ (gameState.firstSpace + 1)
                            , (List.length gameState.indexedMap - gameState.firstSpace) 
                            ] gameState.indexedMap
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

keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map (toKey >> KeyPressed) (Decode.field "key" Decode.string)

toKey : String -> Key
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            Character char

        _ ->
            Control string

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
    Sub.batch [ receiveScrambled (Decode.decodeValue valueStrDecoder >> Received)
              , Browser.Events.onKeyDown keyDecoder
              ]

---- VIEW ----


view : Model -> Html Msg
view model =
    case model of 
        Loading gameOptions ->
            div [] [ Html.text "Loading" ]

        Running gameState ->
            case gameState.wrongGuesses of
            {-
                20 ->
                    viewGameOver gameState
            -}
                _ ->
                    viewGameState gameState


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

displayIfTrue : Bool -> Html Msg
displayIfTrue bool =
        case bool of
            False ->
                div [] []
            True ->
                button [ Html.Attributes.id "nextButton" ] [ Html.text "Next" ]

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

checkIfRevealedHtml : (Int, String) -> Int -> Html msg
checkIfRevealedHtml tuple revealed =
    if revealed > Tuple.first tuple then
        li [ Html.Attributes.class "ltrRevealed" ] [ Html.text (Tuple.second tuple) ]
    else
        li [ Html.Attributes.class "ltrHidden" ] [ Html.text " " ]

checkIfRevealedHtmlBS : (Int, String) -> Int -> Grid.Column msg
checkIfRevealedHtmlBS tuple revealed =
    if revealed > Tuple.first tuple then
        Grid.col [ Col.attrs [ class "ltrRevealed" ] ] [ Html.text (Tuple.second tuple) ]
    else
        case tuple of
        (_, " ") ->
            Grid.col [ Col.attrs [ class "spaceHidden" ] ] [ Html.text " " ]
        _ ->
            Grid.col [ Col.attrs [ class "ltrHidden" ] ] [ Html.text " " ]

        

viewLetter : Int -> (Int, String) -> Html msg
viewLetter revealed indexedLetter =
    div [ Html.Attributes.class "grid-item" ] 
    [ checkIfRevealedHtml indexedLetter revealed ]
{--
viewRow : List (Int, String) -> Html msg
viewRow indexedLetter =
    div [ Html.Attributes.class "listLetterBox" ] 
    [ li [ Html.Attributes.class "listLetter" ] [ Html.text (Tuple.second indexedLetter) ] ]
-}
    --div [] [ checkIfRevealed indexedLetter ]
    --Show either nothing or the letter

viewSimplerKeyboard : GameState -> Html msg
viewSimplerKeyboard gameState =
    Grid.container [] [ Grid.row [] (viewIndexedWordOne gameState)
           , Grid.row [] (viewIndexedWordTwo gameState)
           , Grid.row [] (viewIndexedWordThree gameState)
           , Grid.row [] (viewIndexedWordFour gameState)
           , Grid.row [] (viewIndexedWordFive gameState) 
    ]

viewIndexedWordOne : GameState -> List (Grid.Column msg)
viewIndexedWordOne gameState =
    List.map (flip checkIfRevealedHtmlBS gameState.revealed) gameState.indexedWordOne 

viewIndexedWordTwo : GameState -> List (Grid.Column msg)
viewIndexedWordTwo gameState =
    List.map (flip checkIfRevealedHtmlBS gameState.revealed) gameState.indexedWordTwo 

viewIndexedWordThree : GameState -> List (Grid.Column msg)
viewIndexedWordThree gameState =
    List.map (flip checkIfRevealedHtmlBS gameState.revealed) gameState.indexedWordThree

viewIndexedWordFour : GameState -> List (Grid.Column msg)
viewIndexedWordFour gameState =
    List.map (flip checkIfRevealedHtmlBS gameState.revealed) gameState.indexedWordFour

viewIndexedWordFive : GameState -> List (Grid.Column msg)
viewIndexedWordFive gameState =
    List.map (flip checkIfRevealedHtmlBS gameState.revealed) gameState.indexedWordFive

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
    --|> createInnerList
    |> Html.ul [ Html.Attributes.class "container" ]
{-
createInnerList : List(Html msg) -> List(Html msg)
createInnerList outerList =
    List.map addBrTo outerList
-}
{- This is adding them to each letter
addBrTo : Html msg -> Html msg
addBrTo html =
    div [ Html.Attributes.class "row" ] [ html
           , br [] []
           ]
-}

viewGameOver : GameState -> Html Msg
viewGameOver gameState =
        div [] [ 
            div [] [ h1 [] [ Html.text gameState.displayMsg ]
                   , br [] []
                   , br [] []
                   , button [ onClick Restart ] [ Html.text "Play Again" ]
                   , br [] []
                   , div [ Html.Attributes.id "errorMsg" ] [ Html.text gameState.errorMsg ] ]
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

        scoreHtml =
            div [ Html.Attributes.id "scoreHtml" ] 
                [ span [] [ Html.text "Score:  " ]
                , span [] [ Html.text (String.fromInt (gameState.currentLevel - 1)) ] 
                ]

        wrongGuessesHtml =
            div [ Html.Attributes.id "wrongGuesses" ] 
                [ span [] [ Html.text "Number of Misses:  " ]
                , span [] [ Html.text (String.fromInt gameState.wrongGuesses) ] 
                ]
{-
        keyboardHtml =
            gameState.wordList
            |> List.map (\word ->
                --span [ Html.Attributes.class "keyLetter" ] [ Html.text <| checkIfRevealed char <| getIndex gameState ]
                div [ Html.Attributes.id "keyboardWord" ] [ wordHtml ]
                )
            |> div [ Html.Attributes.id "wholeKeyboard" ]

        keyboardHtml =
            gameState.indexedWordMap
            |> viewKeyboard gameState.revealed
            |> extractHtmlList
            |> getInnerList
-}
        keyboardHtml =
            gameState
            |> viewSimplerKeyboard

        wordHtml =
            gameState.indexedMap
            |> List.map (\tuple ->
                --span [ Html.Attributes.class "keyLetter" ] [ Html.text <| checkIfRevealed char <| getIndex gameState ]
                span [ Html.Attributes.class "keyLetter" ] [ Html.text <| checkIfRevealed tuple gameState.revealed ]
                )
            |> div [ Html.Attributes.class "letterBtns" ]

        nextButtonHtml =
            gameState.nextButton
            |> displayIfTrue

    in
    div []
        [ h1 [] [ Html.text "Guess The Phrase" ]
        , h3 [ Html.Attributes.class "" ] [ Html.text "Can you correctly unscramble the phrase? Beware the CAPITAL LETTERS." ]
        , h4 [] [Html.text "Orange keys are spaces."]
        , br [] []
        , h1 [ Html.Attributes.class "scrambledPhrase" ] [ Html.text gameState.scrambledPhrase ]
        , br [] []
        , br [] []
        , br [] []
        , scoreHtml
        , br [] []
        , wrongGuessesHtml
        , br [] []
        , h4 [] [ Html.text "Keyboard" ]
        , keyboardHtml
        , nextButtonHtml
        , div [ Html.Attributes.id "errorMsg" ] [ Html.text gameState.errorMsg ]
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
