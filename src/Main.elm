module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (on, onClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Extra
import Selection exposing (Selection)


type Attribute
    = Highlight
    | Bold


type
    Doc
    -- TODO: should this be an array?
    = Doc (List Blot)


type Blot
    = Text String
    | Formatted Attribute (List Blot)


type alias EncodeTokens =
    List ( String, List Attribute )


type alias Tokens =
    -- TODO: should we use a set instead of list?
    List ( Char, List Attribute )


type alias Model =
    { selection : Selection
    , doc : Doc
    }


type Msg
    = NoOp
    | ChangeSelection Int Int
    | AddAttribute Attribute
    | RemoveAttribute Attribute


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \model ->
                { title = "Elm Highlight"
                , body = [ view model ]
                }
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { selection = Selection.init 0 0
      , doc = sampleDoc
      }
    , Cmd.none
    )


sampleDoc : Doc
sampleDoc =
    Doc
        [ Text
            (String.repeat 5
                "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. "
            )
        ]


doc2Html : Doc -> List (Html msg)
doc2Html (Doc blots) =
    List.map
        viewBlot
        blots


viewBlot : Blot -> Html msg
viewBlot blot =
    case blot of
        Text text ->
            Html.text text

        Formatted attribute children ->
            case attribute of
                Bold ->
                    Html.span [ Attr.class "bold" ] (List.map viewBlot children)

                Highlight ->
                    Html.span [ Attr.class "highlight" ] (List.map viewBlot children)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangeSelection start end ->
            ( { model | selection = Selection.init start end }
            , Cmd.none
            )

        AddAttribute attribute ->
            ( { model
                | doc = addAttribute attribute model.selection model.doc
              }
            , Cmd.none
            )

        RemoveAttribute attribute ->
            ( { model
                | doc = removeAttribute attribute model.selection model.doc
              }
            , Cmd.none
            )


toTokens : Doc -> Tokens
toTokens (Doc blots) =
    let
        blotTokens blot =
            case blot of
                Text text ->
                    text
                        |> String.toList
                        |> List.map (\c -> ( c, [] ))

                Formatted attribute children ->
                    children
                        |> List.concatMap blotTokens
                        |> List.map
                            (\( c, attributes ) ->
                                -- NOTE: if we do things right, we probably
                                -- shouldn't care about `attribute` being
                                -- present on the recursive result.
                                -- TODO: double check this.
                                ( c, attribute :: attributes )
                            )
    in
    List.concatMap
        blotTokens
        blots


fromTokens : Tokens -> Doc
fromTokens tokens =
    Doc (parseTokens tokens)


parseTokens : Tokens -> List Blot
parseTokens tokens =
    tokens
        |> List.map (\value -> ( highestPriorityAttribute value, value ))
        |> List.Extra.groupWhile (\a b -> Tuple.first a == Tuple.first b)
        |> List.map
            (\( ( maybeAttribute, first ), rest ) ->
                let
                    tokens_ =
                        first :: List.map Tuple.second rest
                in
                case maybeAttribute of
                    Nothing ->
                        tokens_
                            |> List.map Tuple.first
                            |> String.fromList
                            |> Text

                    Just attribute ->
                        Formatted attribute
                            (tokens_
                                |> List.map
                                    (\( c, attrs ) ->
                                        ( c, List.filter (\attr -> attr /= attribute) attrs )
                                    )
                                |> parseTokens
                            )
            )


toEncodeTokens : Tokens -> EncodeTokens
toEncodeTokens tokens =
    tokens
        |> List.Extra.groupWhile
            (\a b ->
                Tuple.second a == Tuple.second b
            )
        |> List.map
            (\( ( char, attributes ), rest ) ->
                let
                    chars =
                        char :: List.map Tuple.first rest
                in
                ( String.fromList chars, attributes )
            )


attributeOrder : Attribute -> Int
attributeOrder attr =
    case attr of
        Highlight ->
            1

        Bold ->
            2


highestPriorityAttribute : ( Char, List Attribute ) -> Maybe Attribute
highestPriorityAttribute ( c, attrs ) =
    List.Extra.minimumBy attributeOrder attrs


addAttribute : Attribute -> Selection -> Doc -> Doc
addAttribute attribute selection doc =
    let
        start =
            Selection.start selection

        end =
            Selection.end selection

        tokens =
            toTokens doc

        newTokens =
            tokens
                |> List.indexedMap
                    (\index ( char, attrs ) ->
                        if index >= start && index < end then
                            ( char, attribute :: List.filter (\a -> a /= attribute) attrs )

                        else
                            ( char, attrs )
                    )

        bar =
            Debug.log "tokens" tokens

        foo =
            Debug.log "newTokens" newTokens
    in
    Debug.log "fromTokens" (fromTokens newTokens)


removeAttribute : Attribute -> Selection -> Doc -> Doc
removeAttribute attribute selection doc =
    let
        start =
            Selection.start selection

        end =
            Selection.end selection

        tokens =
            toTokens doc

        newTokens =
            tokens
                |> List.indexedMap
                    (\index ( char, attrs ) ->
                        if index >= start && index < end then
                            ( char, List.filter (\a -> a /= attribute) attrs )

                        else
                            ( char, attrs )
                    )
    in
    fromTokens newTokens


view : Model -> Html Msg
view model =
    Html.main_
        []
        [ Html.p [] [ Html.text (Debug.toString model.selection) ]
        , viewToolbar
        , Html.node "elm-highlight"
            [ Attr.property "content" (encodeDoc model.doc)
            , on "select" decodeSelect
            ]
            (doc2Html model.doc)
        , Html.p [] [ Html.text "Foobar" ]
        ]


encodeDoc : Doc -> Encode.Value
encodeDoc doc =
    doc
        |> toTokens
        |> toEncodeTokens
        |> Debug.toString
        |> Encode.string


viewToolbar : Html Msg
viewToolbar =
    Html.p []
        [ Html.button [ onClick (AddAttribute Highlight) ] [ Html.text "Add Highlight" ]
        , Html.button [ onClick (RemoveAttribute Highlight) ] [ Html.text "Remove Highlight" ]
        , Html.button [ onClick (AddAttribute Bold) ] [ Html.text "Add Bold" ]
        , Html.button [ onClick (RemoveAttribute Bold) ] [ Html.text "Remove Bold" ]
        ]


decodeSelect : Decoder Msg
decodeSelect =
    Decode.map2 ChangeSelection
        (Decode.at [ "detail", "start", "offset" ] Decode.int)
        (Decode.at [ "detail", "end", "offset" ] Decode.int)
