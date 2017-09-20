module Tests exposing (..)

import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text, tag, attribute, classes)
import Expect
import Fuzz exposing (list, int, tuple, string, Fuzzer)
import String
import Random.Pcg as Random
import Shrink
import Kommandozentrale exposing (ItemEntry, Module(..), MusicPlayerState, BinaryLightState)
import Kommandozentrale exposing (viewItem)
import Char
import Array

itementryFuzz : Bool -> Fuzzer ItemEntry
itementryFuzz unk =
    Fuzz.map3 ItemEntry string string (moduleFuzz unk)

itementryMusicPlayerFuzz : Bool -> Fuzzer ItemEntry
itementryMusicPlayerFuzz unk =
    Fuzz.map3 ItemEntry string string (Fuzz.custom generateMusicPlayer shrinkModule)

moduleFuzz : Bool -> Fuzzer Module
moduleFuzz unk =
    Fuzz.custom (generateModule unk) shrinkModule

nth : Int -> List a -> Maybe a                                                         
nth i xs = Array.fromList xs |> Array.get i

generateModule : Bool -> Random.Generator Module
generateModule unk = 
    let
        generators =
            if unk then 
                [generateBinaryLight, generateMusicPlayer, generateUnknown]
            else
                [generateBinaryLight, generateMusicPlayer]
    in
        Random.int 0 (
            (List.length generators)-1) 
                |> Random.andThen (\b -> nth b generators |> makeModuleJust)

makeModuleJust : Maybe (Random.Generator Module) -> Random.Generator Module
makeModuleJust m =
    case m of
        Just g -> g
        Nothing -> Random.constant Unknown

generateUnknown : Random.Generator Module
generateUnknown = Random.constant Unknown

generateMusicPlayer : Random.Generator Module
generateMusicPlayer =
    Random.maybe Random.bool (Random.map2 MusicPlayerState generateString generateString) |> Random.map MusicPlayer

generateBinaryLight : Random.Generator Module
generateBinaryLight =
    Random.sample [Nothing, Just True, Just False] 
    |> Random.map (Maybe.withDefault Nothing)
    |> Random.map BinaryLight

generateString : Random.Generator String
generateString = 
    Random.list 100 (Random.int 32 126) 
    |> Random.map (
        \l -> List.map (
            \n -> Char.fromCode n
        ) l
    )
    |> Random.map String.fromList

shrinkItemEntry : Shrink.Shrinker ItemEntry
shrinkItemEntry { name, mqtt_id, mod } = 
    Shrink.map ItemEntry (Shrink.string name) 
    |> Shrink.andMap (Shrink.string mqtt_id)
    |> Shrink.andMap (shrinkModule mod)

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
shrinkMusicPlayerState {current_song, playlist_name} = 
    Shrink.map MusicPlayerState (Shrink.string current_song)
        |> Shrink.andMap (Shrink.string playlist_name)

all : Test
all =
    describe "Sample Test Suite"
        [ describe "Functions cover all Types"
            [ fuzz (itementryFuzz False) "non-Unkown Item-Entries always produce <button>" <|
                \ie -> 
                    viewItem ie
                    |> Query.fromHtml
                    |> Query.has [ tag "button" ]

            , fuzz (itementryMusicPlayerFuzz False) "MusicPlayers always show information" <|
                \ie -> 
                    viewItem ie
                    |> Query.fromHtml
                    |> textMusicPlayerShowsInformation ie
            ]
        ]

textMusicPlayerShowsInformation : ItemEntry -> Query.Single -> Expect.Expectation
textMusicPlayerShowsInformation ie = 
    let
        state = case ie.mod of
            MusicPlayer s -> s
            _ -> Debug.crash "textMusicPlayerShowsInformation called with non-MusicPlayer ItemEntry"

        s = Maybe.withDefault {current_song = "No song playing", playlist_name = "No playlist playing"} state

        classlist = [ "btn"
                    , "btn-music"] 
                        ++ case state of
                            Just _ -> []
                            Nothing -> ["disabled"] 
    in
        Query.has [ tag "button"
                  , attribute "type" "button"
                  , classes classlist
                  , text (s.playlist_name)
                  --, text (s.current_song)
                  ]