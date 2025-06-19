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


type alias Model =
    { files : List File
    , activeFile : Maybe File
    , errors : List String
    }


type alias File =
    { id : String
    , name : String
    , parent : String
    , data : Maybe String
    , isOpen : Bool
    }


type Msg
    = Toggle String
    | Open String
    | GotFS (Result Http.Error (List File))


init : () -> ( Model, Cmd Msg )
init _ =
    ( { files = [], activeFile = Nothing, errors = [] }
    , Http.get { url = "/api/v1/files", expect = Http.expectJson GotFS fsDecoder }
    )


fsDecoder : D.Decoder (List File)
fsDecoder =
    D.list <|
        D.map5 File
            (D.field "id" D.string)
            (D.field "name" D.string)
            (D.field "parent" D.string)
            (D.maybe (D.field "data" D.string))
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


toggle : List File -> String -> List File
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
                [ viewFile model.activeFile ]
            ]
        ]
    ]


viewTree : List File -> String -> Html Msg
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
                        viewTree files file.id
                    ]
            )
            (children files parent)


viewFile : Maybe File -> Html Msg
viewFile file =
    div [ class "file-preview" ]
        (case file of
            Nothing ->
                [ text "Select a file" ]

            Just file_ ->
                case file_.data of
                    Nothing ->
                        [ text "Empty file" ]

                    Just content ->
                        [ img
                            [ src content
                            , alt file_.name
                            , class "file-preview__image"
                            , width 200
                            , height 200
                            ]
                            []
                        ]
        )


children : List File -> String -> List File
children files parent =
    List.filter (\file -> file.parent == parent) files
