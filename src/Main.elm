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
    , displayName : Maybe String
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
        |> optional "displayName" (maybe string) Nothing
        |> required "uid" string

groupDecoder =
    Decode.succeed Group
        |> required "dn" string
        |> required "cn" string

type Model = Empty | Loading | Error Http.Error | ReadyUser (List User) | ReadyGroup (List Group)

type Msg = GotUsers (Result Http.Error (List User)) | GetUsers |
    GotGroups (Result Http.Error (List Group)) | GetGroups

update msg model =
    case msg of
        GotUsers result ->
            case result of 
                Ok users -> (ReadyUser users, Cmd.none)
                Err e -> (Error e, Cmd.none)
        GetUsers ->
            (Loading, getUsers)
        GotGroups result ->
            case result of 
                Ok groups -> (ReadyGroup groups, Cmd.none)
                Err e -> (Error e, Cmd.none)
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
              Error e -> div [] [text "Error: ", viewError e]
              ReadyUser users -> 
                  ul [] (List.map viewUser users)
              ReadyGroup groups ->
                  ul [] (List.map viewGroup groups)
        ]

getActiveAttributes : List (String, Maybe String) -> List (String, String)
getActiveAttributes attrs =
    let isActive (label, v) enabled = case v of
                                          Just value -> (label, value) :: enabled
                                          _ -> enabled
    in
        List.foldl isActive [] attrs |> List.reverse

viewAttribute (label, value) = li [] [div [] [text (label ++ ": "), text value]]
            
viewUser user =
    let
        attrs = [("dn", Just user.dn), ("cn", Just user.cn), ("displayName", user.displayName)]
    in
    details []
        [ summary [] [text user.uid]
        , ul []
            (List.map viewAttribute (getActiveAttributes attrs))
        ]

viewGroup group =
    details []
        [ summary [] [text group.cn]
        , ul []
            [(viewAttribute ("dn", group.dn))]
        ]

viewError error =
    case error of
        Http.BadUrl s -> text s
        Http.BadBody s -> text s
        Http.Timeout -> text "timeout"
        Http.NetworkError -> text "network error"
        Http.BadStatus _ -> text "bad status"
        
subscriptions model = Sub.none
                                       
init () = (Empty, getUsers)
    
main = Browser.element
       { init = init
       , update = update
       , subscriptions = subscriptions
       , view = view
       }
