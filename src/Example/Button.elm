module Example.Button exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN

-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    Int


init : Model
init =
    0



-- UPDATE


type Msg
    = Increment
    | Decrement
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1

        Reset ->
            0



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ CDN.stylesheet
        , Button.button [ Button.primary, Button.attrs [ onClick Decrement ] ] [ text "-" ]
        , div [] [ text (String.fromInt model) ]
        , Button.button [ Button.primary, Button.attrs [ onClick Increment ] ] [ text "+" ]
        , Button.button [ Button.primary, Button.attrs [ onClick Reset ] ] [ text "Reset" ]
        ]
