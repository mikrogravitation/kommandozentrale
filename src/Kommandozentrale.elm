port module Kommandozentrale exposing (..)

import Html exposing (..)
--import Html.App as App
import Html.Attributes exposing (placeholder, type_, class, classList, attribute, id)
import Html.Events exposing (onInput, onClick)
import String
import List
import Json.Decode exposing (..)
import Http
import Task
import Regex
import Char

main =
  Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


--- Models
type alias Model =
  { config : Config,
    connected: Bool }

type alias Config =
  { name: String,
    rooms: List Room }

type alias Room =
  { name: String,
    items: List ItemEntry }

type alias ItemEntry =
  { name : String,
    mqtt_id: String,
    mod: Module }

type Module =
  BinaryLight (Maybe BinaryLightState)
  | MusicPlayer (Maybe MusicPlayerState)
  | Unknown

type alias BinaryLightState =
  Bool

type alias MusicPlayerState =
  {current_song: String,
   playlist_name: String}

type Msg =
  ConfigLoadingFinished (Result Http.Error Config) |
--ConfigLoadError Http.Error |
  ItemState String Module |
  MQTTMessage Mqtt_Message |
  SendMessage Mqtt_Message |
  ConnectionChange Bool

type alias Mqtt_Message =
  { mqtt_id: String,
    payload: List Int}
--- init

initialModel : Model
initialModel = { config = { rooms = [], name = "Loading..." }, connected = False }

init : ( Model, Cmd Msg )
init =
    ( initialModel, getConfig "config.json")

--- functions
getConfig : String -> Cmd Msg
getConfig url = Http.send ConfigLoadingFinished (Http.get url decodeConfig)



decodeConfig : Decoder Config
decodeConfig =
  map2 Config
    (field "name" string)
    (field "rooms" (
      list (
        map2 Room
          (field "name"  string)
          (field "items" (list (
            map3 ItemEntry
              (field "name" string)
              (field"mqtt_id"  string)
              ((field "module" string) |> andThen decodeModuleState)
          )))
        )
      )
    )

decodeModuleState : String -> Decoder Module
decodeModuleState module_string =
  let
    args = String.split " " module_string
  in
    case List.head args of
      Nothing -> fail ("Not enough arguments")

      Just "binarylight" ->
        succeed (BinaryLight Nothing)

      Just "musicplayer" ->
        succeed (MusicPlayer Nothing)

      _ -> succeed Unknown

parseLightStateInt: List Int -> Maybe BinaryLightState
parseLightStateInt args =
  case args of
    v::_ ->
      if v == 1 then
          Just True
        else if v == 0 then
          Just False
      else Nothing
    _ -> Nothing

parseMusicPlayerStateInt : List Int -> Maybe MusicPlayerState
parseMusicPlayerStateInt payload =
  let
    json_state = String.fromList (List.map Char.fromCode payload)
    state = Json.Decode.decodeString decodeMusicPlayerState json_state
  in
    case state of
      Ok a -> Just a
      Err e -> Debug.log e Nothing

decodeMusicPlayerState : Decoder MusicPlayerState
decodeMusicPlayerState =
  map2 MusicPlayerState
    (field "current_song" string)
    (field "playlist_name" string)

decodeMsgWithItem : Mqtt_Message -> ItemEntry -> Maybe Module
decodeMsgWithItem message item =
  if item.mqtt_id == message.mqtt_id then
    case item.mod of
      BinaryLight _ -> Just (BinaryLight (parseLightStateInt message.payload))
      MusicPlayer _ -> Just (MusicPlayer (parseMusicPlayerStateInt message.payload))
      _ -> Nothing
  else
    Nothing

decodeMQTTMsg : Mqtt_Message -> Model -> Maybe Module
decodeMQTTMsg message model =
  let
    filtered = List.head (List.filterMap (
      \room ->
        let
          res = List.filterMap (decodeMsgWithItem message) room.items
        in
          if res /= [] then
            Just res
          else
            Nothing
      ) model.config.rooms)
  in
    case filtered of
      Just f ->
        List.head f
      Nothing -> Nothing

itemState : Module -> String -> ItemEntry -> ItemEntry
itemState state mqtt_id item =
  if mqtt_id == item.mqtt_id then
    { item | mod = state }
  else
    item

itemStateConfig : Module -> String -> List Room -> List Room
itemStateConfig state mqtt_id rooms =
 List.map (\room ->
    { room | items =
      List.map (itemState state mqtt_id) room.items
    }
  ) rooms
--- update
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ConfigLoadingFinished (Ok config) ->
      ( { model | config = config}, init_mqtt True )
    ConfigLoadingFinished (Err error) ->
     let
        a = Debug.log "ConfigLoadError" error
        cfg = model.config
        cfgn = { cfg | name = "Config could not be loaded" }
      in
        ( {model | config = cfgn }, Cmd.none )

    MQTTMessage message ->
        let
          state = decodeMQTTMsg message model
          a = case state of
            Just st -> Task.perform identity (Task.succeed (ItemState message.mqtt_id st))
            Nothing -> Cmd.none -- unknown MQTT Type, just ignore

        in
          ( model, a )

    ItemState mqtt_id state ->
      let
        rms = itemStateConfig state mqtt_id model.config.rooms
        cfg = model.config
        cfgn = {cfg | rooms = rms}
      in
        ({ model | config = cfgn }, Cmd.none)

    SendMessage message ->
      (model, send_message message)

    ConnectionChange conn_state ->
      ({model | connected = conn_state}, Cmd.none)
encodeBool : Bool -> List Int
encodeBool bool =
  case bool of
    True -> [1]
    False -> [0]
--- view
view : Model -> Html Msg
view model =
  div [] [
    viewNavBar model,
    div [class "container"] [viewContent model]
  ]

viewNavBar : Model -> Html Msg
viewNavBar model =
  nav [class "navbar navbar-default navbar-static-top container-fluid", attribute "role" "navigation"] [
    div [class "navbar-brand"] [text model.config.name],
    div [class "navbar-brand navbar-right"] [viewConnected model.connected]
  ]

viewConnected : Bool -> Html Msg
viewConnected conn =
  let
    (style, conn_text) =
      if conn then
        ("color:#3fb618", "Connected")
      else
        ("color:#ee1122", "NOT Connected")

  in
    span [attribute "style" style] [text conn_text]

viewContent : Model -> Html Msg
viewContent model = div [] (List.map viewRoom model.config.rooms)

viewRoom : Room -> Html Msg
viewRoom room =
  div [
    classList [("room", True)]
  ] [
    h2 [] [text room.name],
    div [] (List.map viewItem room.items)
  ]

viewItem : ItemEntry -> Html Msg
viewItem item =
  case item.mod of
    BinaryLight state ->
        viewBinaryLight item state

    MusicPlayer state ->
        viewMusicPlayer item state

    _ -> text (toString item)

viewBinaryLight : ItemEntry -> Maybe Bool -> Html Msg
viewBinaryLight light state =
  let
    on = Maybe.withDefault False state
    disabled = case state of
      Just _ -> False
      Nothing -> True

  in
    button [ type_ "button", classList [
      ("btn", True),
      ("btn-light", True),
      ("on", on),
      ("disabled", disabled)],
      onClick (SendMessage { mqtt_id = light.mqtt_id, payload = encodeBool (not on)})
    ] [text light.name]

viewMusicPlayer : ItemEntry -> Maybe MusicPlayerState -> Html Msg
viewMusicPlayer item state =
  let
    disabled = case state of
      Just _ -> False
      Nothing -> True

    s = Maybe.withDefault {current_song = "No song playing", playlist_name = "No playlist playing"} state
  in
    button [ type_ "button", classList [
      ("btn btn-music", True),
      ("disabled", disabled)]
    ] [text ("Current Playlist: " ++ s.playlist_name),
       br [] [],
       text ("Current Song: " ++ s.current_song)]


--- ports
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [
  mqtt_message MQTTMessage,
  connection_change ConnectionChange]

port mqtt_message : (Mqtt_Message -> msg) -> Sub msg
port connection_change : (Bool -> msg) -> Sub msg

port init_mqtt : Bool -> Cmd msg
port send_message : Mqtt_Message -> Cmd msg
