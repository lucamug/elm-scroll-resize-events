port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Regex


port scrollTop : Int -> Cmd msg


port scrollOrResize : (ScreenData -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ scrollOrResize OnScroll
        ]


type alias ScreenData =
    { scrollTop : Int
    , pageHeight : Int
    , viewportHeight : Int
    , viewportWidth : Int
    }


type alias Model =
    { screenData : Maybe ScreenData
    }


model : Model
model =
    { screenData = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


type Msg
    = ScrollTop Int
    | OnScroll ScreenData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ScrollTop position ->
            ( model, Cmd.batch [ scrollTop 0 ] )

        OnScroll data ->
            ( { model | screenData = Just data }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        percentage100 =
            case model.screenData of
                Just data ->
                    perc 100 data

                Nothing ->
                    "0"
    in
        div []
            [ div [ style [ ( "margin", "2em" ) ] ]
                [ h1 [] [ text "Elm - Scroll & Resize Events" ]
                , p [] [ text "Scroll the page and resize the browser to see how these events are captured in Javascript and sent to Elm" ]
                , p []
                    [ text "Article: "
                    , a [ href "https://medium.com/@l.mugnaini/scroll-and-resize-events-in-elm-ac4f0589f42" ] [ text "https://medium.com/@l.mugnaini/scroll-and-resize-events-in-elm-ac4f0589f42" ]
                    ]
                , p []
                    [ text "Code: "
                    , a [ href "https://github.com/lucamug/elm-scroll-resize-events" ] [ text "https://github.com/lucamug/elm-scroll-resize-events" ]
                    ]
                , div [] (List.repeat 15 (p [] [ text """
            Nel mezzo del cammin di nostra vita
            mi ritrovai per una selva oscura,
            ché la diritta via era smarrita.
            Ahi quanto a dir qual era è cosa dura
            esta selva selvaggia e aspra e forte
            che nel pensier rinova la paura!
            Tant' è amara che poco è più morte;
            ma per trattar del ben ch'i' vi trovai,
            dirò de l'altre cose ch'i' v'ho scorte.
            Io non so ben ridir com' i' v'intrai,
            tant' era pien di sonno a quel punto
            che la verace via abbandonai.
            Ma poi ch'i' fui al piè d'un colle giunto,
            là dove terminava quella valle
            che m'avea di paura il cor compunto,
            guardai in alto e vidi le sue spalle
            vestite già de' raggi del pianeta
            che mena dritto altrui per ogne calle.
            Allor fu la paura un poco queta,
            che nel lago del cor m'era durata
            la notte ch'i' passai con tanta pieta.""" ]))
                ]
            , div
                [ style
                    [ ( "position", "fixed" )
                    , ( "top", "0px" )
                    , ( "background-color", "rgb(" ++ percentage100 ++ "%, 0%, 50%)" )
                    , ( "height", "20px" )
                    , ( "width", percentage100 ++ "%" )
                    , ( "transition", "width 0.4s" )
                    , ( "opacity", "0.8" )
                    ]
                ]
                []
            , div
                [ style
                    [ ( "position", "fixed" )
                    , ( "top", "50%" )
                    , ( "left", "50%" )
                    , ( "background-color", "rgb(" ++ percentage100 ++ "%, 0%, 50%)" )
                    , ( "color", "white" )
                    , ( "width", "200px" )
                    , ( "padding", "1em" )
                    , ( "transform", "translate(-50%, -50%)" )
                    , ( "text-align", "center" )
                    , ( "transition", "all 600ms" )
                    , ( "opacity", "0.8" )
                    ]
                ]
                [ div []
                    [ pre []
                        [ case model.screenData of
                            Just data ->
                                text
                                    ((model
                                        |> toString
                                        |> Regex.replace Regex.All (Regex.regex "{.*{") (\_ -> "")
                                        |> Regex.replace Regex.All (Regex.regex "}.*}") (\_ -> "")
                                        |> Regex.replace Regex.All (Regex.regex ",") (\_ -> "\n")
                                     )
                                        ++ "\nscrollBottom = "
                                        ++ toString (data.pageHeight - data.scrollTop - data.viewportHeight)
                                        ++ "\npercentage = "
                                        ++ percentage100
                                        ++ "%"
                                    )

                            Nothing ->
                                text "Scroll or Resize"
                        ]
                    ]
                , button [ onClick (ScrollTop 0) ] [ text "Click here to go to the top" ]
                ]
            ]


percFloat : Int -> ScreenData -> Float
percFloat limit data =
    toFloat (data.scrollTop * limit) / toFloat (data.pageHeight - data.viewportHeight)


perc : Int -> ScreenData -> String
perc limit data =
    data
        |> percFloat limit
        |> round
        |> toString


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
