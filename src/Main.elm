module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (on, onClick)
import Json.Decode as Decode exposing (Decoder)
import Selection exposing (Selection)


type Attribute
    = Highlight
    | Bold


type Doc
    = Doc (List ( Char, List Attribute ))


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
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. "
        |> String.repeat 5
        |> String.toList
        |> List.map (\c -> ( c, [] ))
        |> Doc


doc2Html : Doc -> List (Html msg)
doc2Html (Doc charsWithAttrs) =
    charsWithAttrs
        |> List.map
            (\( c, attrs ) ->
                Html.span (formatAttrs attrs) [ Html.text (String.fromList [ c ]) ]
            )


formatAttrs : List Attribute -> List (Html.Attribute msg)
formatAttrs attrs =
    attrs
        |> List.map
            (\attr ->
                case attr of
                    Highlight ->
                        Attr.class "highlight"

                    Bold ->
                        Attr.class "bold"
            )


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


addAttribute : Attribute -> Selection -> Doc -> Doc
addAttribute attribute selection (Doc charsWithAttrs) =
    let
        start =
            Selection.start selection

        end =
            Selection.end selection
    in
    charsWithAttrs
        |> List.indexedMap
            (\index ( char, attrs ) ->
                if index >= start && index < end then
                    ( char, attribute :: List.filter (\a -> a /= attribute) attrs )

                else
                    ( char, attrs )
            )
        |> Doc


removeAttribute : Attribute -> Selection -> Doc -> Doc
removeAttribute attribute selection (Doc charsWithAttrs) =
    let
        start =
            Selection.start selection

        end =
            Selection.end selection
    in
    charsWithAttrs
        |> List.indexedMap
            (\index ( char, attrs ) ->
                if index >= start && index < end then
                    ( char, List.filter (\a -> a /= attribute) attrs )

                else
                    ( char, attrs )
            )
        |> Doc


view : Model -> Html Msg
view model =
    let
        disable event =
            Html.Events.preventDefaultOn event
                (Decode.map
                    (\msg -> ( msg, True ))
                    (Decode.succeed NoOp)
                )
    in
    Html.main_
        []
        [ Html.p [] [ Html.text (Debug.toString model.selection) ]
        , viewToolbar
        , Html.node "elm-highlight"
            [ Attr.attribute "contenteditable" "false"
            , Attr.attribute "spellcheck" "false"
            , Attr.attribute "readonly" "true"
            , disable "paste"
            , disable "cut"
            , disable "keydown"
            , on "select" decodeSelect
            ]
            (doc2Html model.doc)
        , Html.p [] [ Html.text "Foobar" ]
        ]


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
