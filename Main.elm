module Main exposing (..)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type alias Card =
    { name : String
    , deck : String
    }

type alias Model =
    { cards : Array Card
    , configuring : Bool
    }


type Msg
    = Submit
    | Reconfigure
    | AddCard
    | RemoveCard Int
    | UpdateCardName Int String
    | UpdateCardDeck Int String


init : (Model, Cmd Msg)
init =
    ({ cards = Array.fromList [{ name = "", deck = "" }], configuring = True }, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Submit ->
            ({ model | configuring = False }, Cmd.none)
        Reconfigure ->
            ({ model | configuring = True }, Cmd.none)
        AddCard ->
            ({ model | cards = Array.push { name = "", deck = "" } model.cards }, Cmd.none)
        RemoveCard index ->
            let
                before = Array.slice 0 index model.cards
                after = Array.slice (index + 1) (Array.length model.cards) model.cards
            in
                ({ model | cards = Array.append before after }, Cmd.none)
        UpdateCardName index newName ->
            case Array.get index model.cards of
                Nothing ->
                    (model, Cmd.none)
                Just oldCard ->
                    let
                        newCard = { oldCard | name = newName }
                        newCards = Array.set index newCard model.cards
                    in
                        ({ model | cards = newCards }, Cmd.none)
        UpdateCardDeck index newDeck ->
            case Array.get index model.cards of
                Nothing ->
                    (model, Cmd.none)
                Just oldCard ->
                    let
                        newCard = { oldCard | deck = newDeck }
                        newCards = Array.set index newCard model.cards
                    in
                        ({ model | cards = newCards }, Cmd.none)

view : Model -> Html Msg
view model =
    if model.configuring
       then inputForm model.cards
       else printView model.cards

inputForm : Array Card -> Html Msg
inputForm cards =
    div [ class "InputForm" ]
        [ cardInputs cards
        , button [ class "AddRow", onClick AddCard ] [ text "Add Card" ]
        , button [ class "Submit", onClick Submit ] [ text "Submit" ]
        ]

cardInputs : Array Card -> Html Msg
cardInputs cards =
    div [ class "InputForm-cards" ]
        (Array.indexedMap cardInput cards |> Array.toList)

cardInput : Int -> Card -> Html Msg
cardInput index { name, deck } =
    div [ class "InputForm-card" ]
        [ input [ onInput (UpdateCardName index), value name ] []
        , input [ onInput (UpdateCardDeck index), value deck ] []
        , button [ onClick (RemoveCard index) ] [ text "Remove" ]
        ]

printView : Array Card -> Html Msg
printView cards =
    div [ class "PrintView" ]
        [ div
            [ class "PrintView-cardList"
            , style cardListStyle
            ]
            (Array.indexedMap printCard cards |> Array.toList)
        ]

printCard : Int -> Card -> Html Msg
printCard index { name, deck } =
    div [ class "PrintView-card"
        , style (cardStyle (index % 9 == 0))
        ]
        [ div [ class "PrintView-cardName", style cardNameStyle ] [ text name ]
        , div [ class "PrintView-deckName", style cardDeckStyle ] [ text deck ]
        ]

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }

cardListStyle : List (String, String)
cardListStyle =
    []

cardStyle : Bool -> List (String, String)
cardStyle shouldBreak =
    [ ("width", "2.25in")
    , ("height", "3.25in")
    , ("padding", "0.125in")
    , ("box-sizing", "border-box")
    , ("margin", "2px")
    , ("border", "1px solid #666")
    , ("color", "#333")
    , ("font-family", "sans-serif")
    , ("display", "inline-block")
    , ("page-break-after", if shouldBreak then "always" else "avoid")
    ]

cardNameStyle : List (String, String)
cardNameStyle =
    [ ("font-size", "16pt")
    , ("font-weight", "bold")
    , ("margin-bottom", "0.25in")
    ]

cardDeckStyle : List (String, String)
cardDeckStyle =
    [ ("font-size", "14pt")
    ]

