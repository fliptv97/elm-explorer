module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as D


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = \model -> Browser.Document "Explorer" [ view model ]
        }


type alias Model =
    { root : Maybe Node
    , errorMessage : String
    }


type alias Node =
    { id : String
    , name : String
    , isOpen : Bool
    , children : Children
    }


type Children
    = Children (List Node)


type Msg
    = Toggle Int
    | GotFS (Result Http.Error Node)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing ""
    , Http.get { url = "/api/v1/fs", expect = Http.expectJson GotFS fsDecoder }
    )


fsDecoder : D.Decoder Node
fsDecoder =
    D.map4 Node
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.succeed False)
        (D.field "children" <| D.map Children <| D.list <| D.lazy (\_ -> fsDecoder))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle _ ->
            ( model, Cmd.none )

        GotFS result ->
            case result of
                Ok root ->
                    ( { model | root = Just root }, Cmd.none )

                Err error ->
                    case error of
                        Http.BadBody message ->
                            ( { model | errorMessage = message }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "explorer" ]
            [ div [ class "loader", classList [ ( "loader--visible", isNothing model.root ) ] ]
                [ img [ src "loader.svg", alt "", class "loader__icon" ] [] ]
            , div [ class "explorer__header" ]
                [ text ""
                , input [ placeholder "Search" ] []
                ]
            , div [ class "explorer__sidebar" ]
                [ text "" ]
            , div [ class "explorer__content" ]
                [ pre [] [ text model.errorMessage ] ]
            ]
        ]


isNothing : Maybe a -> Bool
isNothing value =
    case value of
        Nothing ->
            True

        Just _ ->
            False
