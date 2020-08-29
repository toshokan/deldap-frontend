module Main exposing (main)

import Html exposing (text, div, button, ul, li, details, summary)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, string, maybe, list, oneOf)
import Json.Decode.Pipeline exposing (required, optional, hardcoded)
import Array
import Browser
import Debug exposing (toString, log)
import Url.Builder exposing (crossOrigin)
import Config

type DirectoryEntry = UserEntry User | GroupEntry Group

getDn entry = case entry of
                  UserEntry u -> u.dn
                  GroupEntry g -> g.dn

type alias Directory = List DirectoryEntry

type alias User =
    { dn : String
    , cn : String
    , displayName : Maybe String
    , uid: String
    , memberships: List String
    }

type alias Group =
    { dn : String
    , cn : String
    , members: List String
    }    
    
userDecoder =
    Decode.succeed User
        |> required "dn" string
        |> required "cn" string
        |> optional "displayName" (maybe string) Nothing
        |> required "uid" string
        |> optional "memberOf" (list string) []

groupDecoder =
    Decode.succeed Group
        |> required "dn" string
        |> required "cn" string
        |> optional "member" (list string) []

userEntryDecoder = Decode.map UserEntry userDecoder
groupEntryDecoder = Decode.map GroupEntry groupDecoder
directoryEntryDecoder = oneOf [userEntryDecoder, groupEntryDecoder]

type Model = Empty | Loading | Error Http.Error | ReadyDirectory Directory

type Msg = GetUsers | GetGroups | GetGroupMembers Group | GetUserMemberships User
    | GotDirectory (Result Http.Error (List DirectoryEntry)) | GotGroupMembers (Result Http.Error (List DirectoryEntry)) | GotUserMemberships (Result Http.Error (List DirectoryEntry))

update msg model =
    case msg of
        GetUsers ->
            (Loading, getUsers)
        GetGroups ->
            (Loading, getGroups)
        GetGroupMembers group -> (model, getGroupMembers group)
        GetUserMemberships group -> (model, getUserMemberships group)
        GotDirectory result ->
            case result of 
                Ok entries -> (ReadyDirectory entries, Cmd.none)
                Err e -> (Error e, Cmd.none)
        GotGroupMembers result ->
            case result of
                Ok (entry :: _) -> ((updateEntry model entry), Cmd.none)
                Ok [] -> (model, Cmd.none)
                Err e -> (Error e, Cmd.none)
        GotUserMemberships result ->
            case result of
                Ok (entry :: _) -> ((updateEntry model entry), Cmd.none)
                Ok [] -> (model, Cmd.none)
                Err e -> (Error e, Cmd.none)

spliceEntry entries newEntry =
    let
        replace entry = if (getDn entry) == (getDn newEntry) then newEntry else entry
    in
        List.map replace entries
                         
updateEntry model entry =
    case model of
        ReadyDirectory entries -> ReadyDirectory (spliceEntry entries entry)
        _ -> model

getUsers =
    let
        dn = Config.userDn
        url = crossOrigin Config.deldapBase ["api", "v1", "children" ] [Url.Builder.string "dn" dn]
    in
    Http.get
        { url = url
        , expect = Http.expectJson GotDirectory (list userEntryDecoder)
        }

getGroups =
    let
        dn = Config.groupDn
        url = crossOrigin Config.deldapBase ["api", "v1", "children" ] [Url.Builder.string "dn" dn]
    in
    Http.get
        { url = url
        , expect = Http.expectJson GotDirectory (list groupEntryDecoder)
        }

getGroupMembers group =
    let
        dn = group.dn
        url = crossOrigin Config.deldapBase ["api", "v1", "members" ] [Url.Builder.string "dn" dn]
    in
        Http.get
            { url = url
            , expect = Http.expectJson GotGroupMembers (list directoryEntryDecoder)
        }

getUserMemberships user =
    let
        dn = user.dn
        url = crossOrigin Config.deldapBase ["api", "v1", "memberships" ] [Url.Builder.string "dn" dn]
    in
        Http.get
            { url = url
            , expect = Http.expectJson GotUserMemberships (list directoryEntryDecoder)
        }

view model =
    div []
        [ button [ onClick GetUsers ] [ text "Fetch users" ]
        , button [ onClick GetGroups ] [ text "Fetch groups" ]
        , case model of
              Empty -> div [] []
              Loading -> text " Loading..."
              Error e -> div [] [text "Error: ", viewError e]
              ReadyDirectory dir ->
                  ul [] (List.map viewDirectoryEntry dir)
        ]

getActiveAttributes : List (String, Maybe String) -> List (String, String)
getActiveAttributes attrs =
    let isActive (label, v) enabled = case v of
                                          Just value -> (label, value) :: enabled
                                          _ -> enabled
    in
        List.foldl isActive [] attrs |> List.reverse

viewAttribute (label, value) = li [] [div [] [text (label ++ ": "), text value]]

viewDirectoryEntry dirent =
    case dirent of
        UserEntry user -> viewUser user
        GroupEntry group -> viewGroup group
            
viewUser user =
    let
        membershipsView = case user.memberships of
                              [] -> []
                              xs -> [li [] [text "Memberships: "], ul [] (List.map (\m -> li [] [div [] [text m]]) xs)]
        attrs = [("dn", Just user.dn), ("cn", Just user.cn), ("displayName", user.displayName)]
    in
    details []
        [ summary [] [text user.uid]
        , ul []
            ((List.map viewAttribute (getActiveAttributes attrs)) ++
                membershipsView ++
            [button [onClick (GetUserMemberships user)] [text "Get Memberships"]])
        ]

viewGroup group =
    let
        memberView = case group.members of
                         [] -> []
                         xs -> [li [] [text "Members: "], ul [] (List.map (\m -> li [] [div [] [text m]]) xs)]
    in
    details []
        [ summary [] [text group.cn]
        , ul []
            ((viewAttribute ("dn", group.dn)) ::
                 memberView ++
            [button [onClick (GetGroupMembers group)] [text "Get Members"]])
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
