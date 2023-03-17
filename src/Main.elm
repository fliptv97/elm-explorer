module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = \model -> Browser.Document "Explorer" (view model)
        }


type alias Files =
    List Node


type alias Model =
    { files : Files
    , activeFile : Maybe Node
    , errors : List String
    }


type alias Node =
    { id : String
    , name : String
    , parent : String
    , isOpen : Bool
    }


type Msg
    = Toggle String
    | Open String
    | GotFS (Result Http.Error (List Node))


init : () -> ( Model, Cmd Msg )
init _ =
    ( { files = [], activeFile = Nothing, errors = [] }
    , Http.get { url = "/api/v1/files", expect = Http.expectJson GotFS fsDecoder }
    )


fsDecoder : D.Decoder (List Node)
fsDecoder =
    D.list <|
        D.map4 Node
            (D.field "id" D.string)
            (D.field "name" D.string)
            (D.field "parent" D.string)
            (D.succeed False)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle id ->
            ( { model | files = toggle model.files id }, Cmd.none )

        Open id ->
            ( { model | activeFile = find (\file -> file.id == id) model.files }, Cmd.none )

        GotFS result ->
            case result of
                Ok files ->
                    ( { model | files = files }, Cmd.none )

                Err error ->
                    case error of
                        Http.BadBody message ->
                            ( { model | errors = model.errors ++ [ message ] }, Cmd.none )

                        _ ->
                            ( { model | errors = model.errors ++ [ "Something went wrong" ] }, Cmd.none )


toggle : Files -> String -> Files
toggle files id =
    List.map
        (\file ->
            if file.id == id then
                { file | isOpen = not file.isOpen }

            else
                file
        )
        files


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    List.head (List.filter predicate list)


view : Model -> List (Html Msg)
view model =
    [ div [ class "notifications" ] <|
        List.map (\notification -> div [ class "notification" ] [ text notification ]) model.errors
    , div [ class "container" ]
        [ div [ class "explorer" ]
            [ div [ class "loader", classList [ ( "loader--visible", List.isEmpty model.files ) ] ]
                [ img [ src "loader.svg", alt "", class "loader__icon" ] [] ]
            , div [ class "explorer__header" ]
                [ text ""
                , input [ placeholder "Search" ] []
                ]
            , div [ class "explorer__sidebar" ]
                [ viewTree model.files "root" ]
            , div [ class "explorer__content" ]
                [ text "" ]
            ]
        ]
    ]


viewTree : Files -> String -> Html Msg
viewTree files parent =
    ul [] <|
        List.map
            (\file ->
                let
                    childrenFiles =
                        children files file.id

                    listener =
                        if List.isEmpty childrenFiles then
                            stopPropagationOn "click" (D.succeed ( Open file.id, True ))

                        else
                            stopPropagationOn "click" (D.succeed ( Toggle file.id, True ))
                in
                li
                    [ classList
                        [ ( "is-opened", file.isOpen )
                        , ( "toggleable", not (List.isEmpty childrenFiles) )
                        ]
                    , listener
                    ]
                    [ text file.name
                    , if List.isEmpty childrenFiles || not file.isOpen then
                        text ""

                      else
                        viewTree childrenFiles file.id
                    ]
            )
            (children files parent)


children : Files -> String -> Files
children files parent =
    List.filter (\file -> file.parent == parent) files
