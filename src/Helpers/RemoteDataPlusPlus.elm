module Helpers.RemoteDataPlusPlus exposing
    ( Haveness(..)
    , ReceivedTime
    , RefreshStatus(..)
    , RemoteDataPlusPlus
    , RequestedTime
    , andMap
    , decoder
    , empty
    , encode
    , gotData
    , isLoading
    , map
    , map2
    , setData
    , setLoading
    , setNotLoading
    , setRefreshStatus
    , toMaybe
    , withDefault
    )

import Json.Decode as Decode
import Json.Encode as Encode
import Time



{-
   Like https://package.elm-lang.org/packages/krisajenkins/remotedata but with blackjack and timestamps
-}


type alias RemoteDataPlusPlus error data =
    { data : Haveness data
    , refreshStatus : RefreshStatus error
    }


type Haveness data
    = DontHave
    | DoHave data ReceivedTime


type RefreshStatus e
    = NotLoading (Maybe ( e, ReceivedTime ))
    | Loading


type alias RequestedTime =
    Time.Posix


type alias ReceivedTime =
    Time.Posix



-- Convenience functions


andMap : RemoteDataPlusPlus e a -> RemoteDataPlusPlus e (a -> b) -> RemoteDataPlusPlus e b
andMap wrappedValue wrappedFunction =
    case ( wrappedFunction.data, wrappedValue.data ) of
        ( DoHave f _, DoHave b t ) ->
            RemoteDataPlusPlus (DoHave (f b) t) wrappedValue.refreshStatus

        ( DontHave, _ ) ->
            RemoteDataPlusPlus DontHave wrappedFunction.refreshStatus

        ( _, DontHave ) ->
            RemoteDataPlusPlus DontHave wrappedValue.refreshStatus


map : (a -> b) -> RemoteDataPlusPlus error a -> RemoteDataPlusPlus error b
map f rdpp =
    let
        newData =
            case rdpp.data of
                DoHave data time ->
                    DoHave (f data) time

                DontHave ->
                    DontHave
    in
    RemoteDataPlusPlus newData rdpp.refreshStatus


map2 : (a -> b -> c) -> RemoteDataPlusPlus error a -> RemoteDataPlusPlus error b -> RemoteDataPlusPlus error c
map2 f a b =
    map f a |> andMap b


withDefault : data -> RemoteDataPlusPlus error data -> data
withDefault default rdpp =
    -- Returns data, or the default
    case rdpp.data of
        DoHave data _ ->
            data

        DontHave ->
            default


empty : RemoteDataPlusPlus x y
empty =
    RemoteDataPlusPlus DontHave (NotLoading Nothing)


setRefreshStatus : RefreshStatus error -> RemoteDataPlusPlus error data -> RemoteDataPlusPlus error data
setRefreshStatus refreshStatus rdpp =
    { rdpp | refreshStatus = refreshStatus }


gotData : RemoteDataPlusPlus error data -> Bool
gotData rdpp =
    case rdpp.data of
        DoHave _ _ ->
            True

        DontHave ->
            False


setData : Haveness data -> RemoteDataPlusPlus error data -> RemoteDataPlusPlus error data
setData haveness rdpp =
    RemoteDataPlusPlus haveness rdpp.refreshStatus


isLoading : RemoteDataPlusPlus error data -> Bool
isLoading rdpp =
    case rdpp.refreshStatus of
        Loading ->
            True

        NotLoading _ ->
            False


setLoading : RemoteDataPlusPlus error data -> RemoteDataPlusPlus error data
setLoading rdpp =
    setRefreshStatus Loading rdpp


setNotLoading : Maybe ( error, ReceivedTime ) -> RemoteDataPlusPlus error data -> RemoteDataPlusPlus error data
setNotLoading error rdpp =
    setRefreshStatus (NotLoading error) rdpp


toMaybe : RemoteDataPlusPlus error data -> Maybe data
toMaybe rdpp =
    case rdpp.data of
        DoHave data _ ->
            Just data

        _ ->
            Nothing


{-| Encode an RDPP to JSON for storage.

    {
        "haveness": <boolean>,
        "data": <data if haveness>,
        "dataReceivedTime": <millis if haveness>,
        "loading": <boolean>,
        "errored": <boolean>,
        "error": <error if errored>,
        "errorReceivedTime": <millis if errored>
    }

-}
encode : (data -> Encode.Value) -> (error -> Encode.Value) -> RemoteDataPlusPlus error data -> Encode.Value
encode encodeData encodeError rdpp =
    let
        ( haveness, dataFields ) =
            case rdpp.data of
                DontHave ->
                    ( False, [] )

                DoHave data receivedTime ->
                    ( True
                    , [ ( "data", encodeData data )
                      , ( "dataReceivedTime", Encode.int (Time.posixToMillis receivedTime) )
                      ]
                    )

        ( loading, errored, errorFields ) =
            case rdpp.refreshStatus of
                Loading ->
                    ( True, False, [] )

                NotLoading Nothing ->
                    ( False, False, [] )

                NotLoading (Just ( error, errorTime )) ->
                    ( False
                    , True
                    , [ ( "error", encodeError error )
                      , ( "errorReceivedTime", Encode.int (Time.posixToMillis errorTime) )
                      ]
                    )
    in
    Encode.object
        ([ ( "haveness", Encode.bool haveness )
         , ( "loading", Encode.bool loading )
         , ( "errored", Encode.bool errored )
         ]
            ++ dataFields
            ++ errorFields
        )


decoder : Decode.Decoder data -> Decode.Decoder error -> Decode.Decoder (RemoteDataPlusPlus error data)
decoder dataDecoder errorDecoder =
    Decode.map2 RemoteDataPlusPlus
        (havenessDecoder dataDecoder)
        (refreshStatusDecoder errorDecoder)


havenessDecoder : Decode.Decoder data -> Decode.Decoder (Haveness data)
havenessDecoder dataDecoder =
    Decode.field "haveness" Decode.bool
        |> Decode.andThen
            (\hasData ->
                if hasData then
                    Decode.map2 DoHave
                        (Decode.field "data" dataDecoder)
                        (Decode.field "dataReceivedTime" Decode.int |> Decode.map Time.millisToPosix)

                else
                    Decode.succeed DontHave
            )


refreshStatusDecoder : Decode.Decoder error -> Decode.Decoder (RefreshStatus error)
refreshStatusDecoder errorDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "loading" Decode.bool)
        (Decode.field "errored" Decode.bool)
        |> Decode.andThen
            (\( loading, errored ) ->
                if loading then
                    Decode.succeed Loading

                else if errored then
                    Decode.map2 (\e t -> NotLoading (Just ( e, t )))
                        (Decode.field "error" errorDecoder)
                        (Decode.field "errorReceivedTime" Decode.int |> Decode.map Time.millisToPosix)

                else
                    Decode.succeed (NotLoading Nothing)
            )
