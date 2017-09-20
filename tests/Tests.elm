module Tests exposing (..)

import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text, tag, attribute, classes, class)
import Expect
import Fuzz exposing (list, int, tuple, string, Fuzzer)
import String
import Random.Pcg as Random
import Shrink
import Kommandozentrale exposing (ItemEntry, Module(..), MusicPlayerState, BinaryLightState, Room, Config)
import Kommandozentrale exposing (viewItem, viewRoom, decodeConfig)
import Char
import Array
import Json.Decode exposing (decodeString)

nth : Int -> List a -> Maybe a
nth i xs =
    Array.fromList xs |> Array.get i

valid_config_json : String
valid_config_json =
    "{ \"name\": \"Test Instance\", \"rooms\": [  {\"name\":\"Room 1\", \"items\": [ {\"name\":\"Item 1\",\"mqtt_id\":\"test_item\",\"module\":\"binarylight\"}]}]}"

corrupt_json : String
corrupt_json =
    "{ \"name\": \"T"

invalid_config_json : String
invalid_config_json =
    "{ \"name\": \"Test Instance\", \"rooms\": [  {\"items\":\"Room 1\", \"name\": [ {\"name\":\"Item 1\",\"mqtt_id\":\"test_item\",\"module\":\"binarylight\"}]}]}"

-- fuzzers
itementryFuzz : Bool -> Fuzzer ItemEntry
itementryFuzz unk =
    Fuzz.map3 ItemEntry string string (moduleFuzz unk)


itementryOneFuzz : Random.Generator Module -> Fuzzer ItemEntry
itementryOneFuzz itemtype =
    Fuzz.map3 ItemEntry string string (Fuzz.custom itemtype shrinkModule)

shrinkItemEntry : Shrink.Shrinker ItemEntry
shrinkItemEntry { name, mqtt_id, mod } =
    Shrink.map ItemEntry (Shrink.string name)
        |> Shrink.andMap (Shrink.string mqtt_id)
        |> Shrink.andMap (shrinkModule mod)

roomFuzz : Bool -> Fuzzer Room
roomFuzz unk =
    Fuzz.map2 Room
        string
        (Fuzz.list (itementryFuzz unk))

moduleFuzz : Bool -> Fuzzer Module
moduleFuzz unk =
    Fuzz.custom (generateModule unk) shrinkModule


generateModule : Bool -> Random.Generator Module
generateModule unk =
    let
        generators =
            if unk then
                [ generateBinaryLight, generateMusicPlayer, generateUnknown ]
            else
                [ generateBinaryLight, generateMusicPlayer ]
    in
        Random.int 0
            ((List.length generators) - 1)
            |> Random.andThen (\b -> nth b generators |> makeModuleJust)


makeModuleJust : Maybe (Random.Generator Module) -> Random.Generator Module
makeModuleJust m =
    case m of
        Just g ->
            g

        Nothing ->
            generateUnknown

generateUnknown : Random.Generator Module
generateUnknown =
    Random.constant Unknown


generateMusicPlayer : Random.Generator Module
generateMusicPlayer =
    Random.maybe Random.bool (Random.map2 MusicPlayerState generateString generateString)
        |> Random.map MusicPlayer


generateBinaryLight : Random.Generator Module
generateBinaryLight =
    Random.sample [ Nothing, Just True, Just False ]
        |> Random.map (Maybe.withDefault Nothing)
        |> Random.map BinaryLight


generateString : Random.Generator String
generateString =
    Random.list 100 (Random.int 32 126)
        |> Random.map
            (\l ->
                List.map
                    (\n -> Char.fromCode n)
                    l
            )
        |> Random.map String.fromList

shrinkModule : Shrink.Shrinker Module
shrinkModule mod =
    case mod of
        BinaryLight mb ->
            Shrink.maybe Shrink.bool mb |> Shrink.map BinaryLight

        MusicPlayer mb ->
            shrinkMaybeMusicPlayerState mb |> Shrink.map MusicPlayer

        Unknown ->
            Shrink.noShrink Unknown


shrinkMaybeMusicPlayerState : Shrink.Shrinker (Maybe MusicPlayerState)
shrinkMaybeMusicPlayerState mb =
    Shrink.maybe shrinkMusicPlayerState mb


shrinkMusicPlayerState : Shrink.Shrinker MusicPlayerState
shrinkMusicPlayerState { current_song, playlist_name } =
    Shrink.map MusicPlayerState (Shrink.string current_song)
        |> Shrink.andMap (Shrink.string playlist_name)

-- tests
testRoomValidModules : Room -> Query.Single -> Expect.Expectation
testRoomValidModules room =
    Query.has [ tag "div" ]


testMusicPlayerShowsInformation : ItemEntry -> Query.Single -> Expect.Expectation
testMusicPlayerShowsInformation ie =
    let
        state =
            case ie.mod of
                MusicPlayer s ->
                    s

                _ ->
                    Debug.crash "testMusicPlayerShowsInformation called with non-MusicPlayer ItemEntry"

        s =
            Maybe.withDefault { current_song = "No song playing", playlist_name = "No playlist playing" } state

        classlist =
            [ "btn"
            , "btn-music"
            ]
                ++ case state of
                    Just _ ->
                        []

                    Nothing ->
                        [ "disabled" ]
    in
        Query.has
            [ tag "button"
            , attribute "type" "button"
            , classes classlist
            , text (s.playlist_name)
            --, text (s.current_song)
            ]


testBinaryLightShowsInformation : ItemEntry -> Query.Single -> Expect.Expectation
testBinaryLightShowsInformation ie =
    let
        state =
            case ie.mod of
                BinaryLight s ->
                    s

                _ ->
                    Debug.crash "testBinaryLightShowsInformation called with non-BinaryLight ItemEntry"

        classlist =
            [ "btn"
            , "btn-light"
            ]
                ++ (case (Maybe.withDefault False state) of
                        True ->
                            [ "on" ]

                        False ->
                            []
                   )
                ++ (case state of
                        Just _ ->
                            []

                        Nothing ->
                            [ "disabled" ]
                   )
    in
        Query.has
            [ tag "button"
            , attribute "type" "button"
            , classes classlist
            , text (ie.name)
            ]


-- test suite
all : Test
all =
    describe "Unit Test Suite"
        [ describe "Fuzz tests"
            [ fuzz (itementryFuzz False) "non-Unkown ItemEntrys always produce <button>" <|
                \ie ->
                    viewItem ie
                        |> Query.fromHtml
                        |> Query.has [ tag "button" ]
            , fuzz (itementryOneFuzz generateMusicPlayer) "MusicPlayers always show information" <|
                \ie ->
                    viewItem ie
                        |> Query.fromHtml
                        |> testMusicPlayerShowsInformation ie
            , fuzz (itementryOneFuzz generateBinaryLight) "BinaryLights always show information" <|
                \ie ->
                    viewItem ie
                        |> Query.fromHtml
                        |> testBinaryLightShowsInformation ie
            , fuzz (roomFuzz False) "Rooms always generate valid modules" <|
                \room ->
                    viewRoom room
                        |> Query.fromHtml
                        |> Query.find [ tag "div" ]
                        |> Query.children []
                        |> Query.each (testRoomValidModules room)
        ]
        , describe "Fixed value tests"
            [ test "Valid config decodes" <|
                \() ->
                    let
                        item = ItemEntry "Item 1" "test_item" (BinaryLight Nothing)
                        room = Room "Room 1" [item]
                        expected_config = Config "Test Instance" [ room ]
                        actual_config = Json.Decode.decodeString decodeConfig valid_config_json
                    in
                        Expect.equal (Ok expected_config) actual_config

            , test "Invalid json fails" <|
                \() ->
                    let
                        actual_config = Json.Decode.decodeString decodeConfig corrupt_json
                    in
                        Expect.equal (Err "Given an invalid JSON: Unexpected end of JSON input") actual_config
            , test "Invalid config fails" <|
                \() ->
                    let
                        actual_config = Json.Decode.decodeString decodeConfig invalid_config_json
                    in
                        case actual_config of
                            Ok _ -> Expect.fail "Corrupt json should not decode with Ok"
                            Err _ -> Expect.pass
            ]
        ]
