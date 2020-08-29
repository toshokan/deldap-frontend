module Main exposing (main)

import Html exposing (text, div, button, ul, li, details, summary)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, string, maybe, list)
import Json.Decode.Pipeline exposing (required, optional)
import Array
import Browser
import Debug exposing (toString)
import Url.Builder exposing (crossOrigin)

type alias User =
    { dn : String
    , cn : String
    , displayName : String
    , uid: String
    }

type alias Group =
    { dn : String
    , cn : String
    }    
    
userDecoder =
    Decode.succeed User
        |> required "dn" string
        |> required "cn" string
        |> required "displayName" string
        |> required "uid" string

groupDecoder =
    Decode.succeed Group
        |> required "dn" string
        |> required "cn" string

type Model = Empty | Loading | Error String | ReadyUser (List User) | ReadyGroup (List Group)

type Msg = GotUsers (Result Http.Error (List User)) | GetUsers |
    GotGroups (Result Http.Error (List Group)) | GetGroups

update msg model =
    case msg of
        GotUsers result ->
            case result of 
                Ok users -> (ReadyUser users, Cmd.none)
                Err e -> (Error "Error loading model", Cmd.none)
        GetUsers ->
            (Loading, getUsers)
        GotGroups result ->
            case result of 
                Ok groups -> (ReadyGroup groups, Cmd.none)
                Err e -> (Error "Error loading model", Cmd.none)
        GetGroups ->
            (Loading, getGroups)

getUsers =
    let
        dn = "cn=users"
        url = crossOrigin "http://localhost:8001" ["api", "v1", "children" ] [Url.Builder.string "dn" dn]
    in
    Http.get
        { url = url
        , expect = Http.expectJson GotUsers (list userDecoder)
        }

getGroups =
    let
        dn = "cn=groups"
        url = crossOrigin "http://localhost:8001" ["api", "v1", "children" ] [Url.Builder.string "dn" dn]
    in
    Http.get
        { url = url
        , expect = Http.expectJson GotGroups (list groupDecoder)
        }        

view model =
    div []
        [ button [ onClick GetUsers ] [ text "Fetch users" ]
        , button [ onClick GetGroups ] [ text "Fetch groups" ]
        , case model of
              Empty -> div [] []
              Loading -> text " Loading..."
              Error s -> div [] [text "Error: ", text s]
              ReadyUser users -> 
                  ul [] (List.map viewUser users)
              ReadyGroup groups ->
                  ul [] (List.map viewGroup groups)
        ]
        

viewUser user =
    details []
        [ summary [] [text user.uid]
        , ul []
            [ li [] [div [] [text "dn: ", text user.dn]]
            , li [] [div [] [text "cn: ", text user.cn]]
            , li [] [div [] [text "displayName: ", text user.displayName]]
            ]
        ]

viewGroup group =
    details []
        [ summary [] [text group.dn]
        , ul []
            [ li [] [div [] [text "cn: ", text group.cn]]
            ]
        ]
        
subscriptions model = Sub.none
                                       
init () = (Empty, getUsers)
    
main = Browser.element
       { init = init
       , update = update
       , subscriptions = subscriptions
       , view = view
       }
