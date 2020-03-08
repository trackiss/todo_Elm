module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Material
import Material.Button as Button
import Material.Dialog as Dialog
import Material.Drawer.Modal as Drawer
import Material.Icon as Icon
import Material.List as Lists
import Material.Options as Options exposing (..)
import Material.TopAppBar as TopAppBar
import Material.Typography as Typography
import Time
import TimeZone exposing (asia__tokyo)
import UUID exposing (UUID)
import Url



-- MAIN


main : Program Int Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type Status
    = Yet
    | Overdue
    | Done
    | Disable


type alias Model =
    { mdc : Material.Model Msg
    , url : Url.Url
    , key : Nav.Key
    , timeZone : Time.Zone
    , todoItemList : List TodoItem
    , tagList : List Tag
    , editingTodoItem : Maybe TodoItem
    , isOpenedDrawer : Bool
    , isOpenedDialog : Bool
    }


defaultModel : Url.Url -> Nav.Key -> Model
defaultModel url key =
    { mdc = Material.defaultModel
    , url = url
    , key = key
    , timeZone = asia__tokyo ()
    , todoItemList = []
    , tagList = []
    , editingTodoItem = Nothing
    , isOpenedDrawer = False
    , isOpenedDialog = False
    }


type alias TodoItem =
    { id : UUID
    , title : String
    , date : Time.Posix
    , status : Status
    , memo : String
    , tagId : UUID
    , createdAt : Time.Posix
    }


type alias Tag =
    { id : UUID
    , name : String
    , countItem : Int
    , countDone : Int
    , createdAt : Time.Posix
    }


init : Int -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( defaultModel url key
    , Cmd.none
    )



-- UPDATE


type Msg
    = Mdc (Material.Msg Msg)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | OpenDrawer
    | CloseDrawer
    | OpenDialog
    | CloseDialog


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdc msg_ ->
            Material.update Mdc msg_ model

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        OpenDrawer ->
            ( { model | isOpenedDrawer = True }
            , Cmd.none
            )

        CloseDrawer ->
            ( { model | isOpenedDrawer = False }
            , Cmd.none
            )

        OpenDialog ->
            ( { model | isOpenedDialog = True }
            , Cmd.none
            )

        CloseDialog ->
            ( { model | isOpenedDialog = False }
            , Cmd.none
            )


toMonthNumber : Time.Month -> Int
toMonthNumber month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "TodoList on Elm"
    , body =
        [ viewTopAppBar model
        , viewDrawer model
        , Drawer.scrim [ Options.onClick CloseDrawer ] []
        , styled Html.main_
            [ TopAppBar.fixedAdjust ]
            [ viewContent model ]
        ]
    }


viewTopAppBar : Model -> Html Msg
viewTopAppBar model =
    TopAppBar.view Mdc
        "top-app-bar"
        model.mdc
        [ TopAppBar.fixed ]
        [ TopAppBar.section
            [ TopAppBar.alignStart ]
            [ TopAppBar.navigationIcon Mdc "top-app-bar-menu" model.mdc [ Options.onClick OpenDrawer ] "menu"
            , TopAppBar.title [] [ text "TodoList on Elm" ]
            ]
        ]


viewDrawer : Model -> Html Msg
viewDrawer model =
    Drawer.view Mdc
        "drawer"
        model.mdc
        [ Drawer.open |> when model.isOpenedDrawer
        , Drawer.onClose CloseDrawer
        ]
        [ Drawer.header []
            [ styled h3 [ Drawer.title ] [ text "Menu" ] ]
        , Drawer.content [] []
        ]


viewContent : Model -> Html Msg
viewContent model =
    {- if List.isEmpty model.todoItemList then
         styled h2
             [ css "color" "silver"
             , css "left" "50%"
             , css "position" "absolute"
             , css "text-align" "center"
             , css "top" "50%"
             , css "transform" "translateY(-50%) translateX(-50%)"
             , Typography.typography
             ]
             [ text "EMPTY" ]

       else
    -}
    styled div
        [ css "border" "1px solid rgba(0,0,0,.1)"
        , css "margin-left" "500px"
        , css "margin-right" "500px"
        , css "margin-top" "50px"
        ]
        [ viewTodoList model
        , viewTodoEdit model
        ]


viewTodoList : Model -> Html Msg
viewTodoList model =
    Lists.ul Mdc
        "todo-list"
        model.mdc
        [ Lists.twoLine ]
        [ Lists.li [ Options.onClick OpenDrawer ]
            [ Lists.text []
                [ Lists.primaryText [] [ text "Test1" ]
                , Lists.secondaryText [] [ text "test " ]
                ]
            ]
        , Lists.li [ Options.onClick OpenDialog ]
            [ Lists.text []
                [ Lists.primaryText [] [ text "Test2" ]
                , Lists.secondaryText [] [ text "test " ]
                ]
            ]
        ]


viewTodoItem : Model -> TodoItem -> Lists.ListItem Msg
viewTodoItem model todoItem =
    Lists.li [ Options.onClick OpenDialog ]
        [ let
            y =
                Time.toYear model.timeZone todoItem.date

            m =
                Time.toMonth model.timeZone todoItem.date |> toMonthNumber

            d =
                Time.toDay model.timeZone todoItem.date
          in
          Lists.text []
            [ case todoItem.status of
                Overdue ->
                    Lists.primaryText [ css "color" "tomato" ] [ text todoItem.title ]

                Disable ->
                    Lists.primaryText [ css "color" "gray" ] [ text todoItem.title ]

                _ ->
                    Lists.primaryText [] [ text todoItem.title ]
            , Lists.secondaryText [] [ text ("until " ++ String.fromInt y ++ "/" ++ String.fromInt m ++ "/" ++ String.fromInt d) ]
            , if todoItem.status == Done then
                Lists.metaIcon [] "done"

              else
                Lists.metaIcon [] ""
            ]
        ]


viewTodoEdit : Model -> Html Msg
viewTodoEdit model =
    Dialog.view Mdc
        "edit-dialog"
        model.mdc
        [ Dialog.open |> when model.isOpenedDialog
        , Dialog.onClose CloseDialog
        ]
        [ styled Html.h2
            [ Dialog.title
            ]
            [ text "Use Google's location service?"
            ]
        , Dialog.content []
            [ text
                """
                 Let Google help apps determine location. This means
                 sending anonymous location data to Google, even when
                 no apps are running.
                 """
            ]
        , Dialog.actions []
            [ Button.view Mdc
                "my-cancel-button"
                model.mdc
                [ Button.ripple
                , Dialog.cancel
                , Options.onClick CloseDialog
                ]
                [ text "Decline"
                ]
            , Button.view Mdc
                "my-accept-button"
                model.mdc
                [ Button.ripple
                , Dialog.accept
                , Options.onClick CloseDialog
                ]
                [ text "Continue"
                ]
            ]
        ]
