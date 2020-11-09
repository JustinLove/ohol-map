port module LocalStorage exposing (save, loaded, saveJson, loadedJson)

import Json.Encode
import Json.Decode

save : String -> Cmd msg
save = localStorageSave

saveJson : Json.Encode.Value -> Cmd msg
saveJson = Json.Encode.encode 0 >> localStorageSave

loaded : (Maybe String -> msg) -> Sub msg
loaded = localStorageLoaded

loadedJson : Json.Decode.Decoder a -> (Maybe a -> msg) -> Sub msg
loadedJson decoder tagger =
  localStorageLoaded ((decodeLoaded decoder) >> tagger)

decodeLoaded : Json.Decode.Decoder a -> Maybe String -> Maybe a
decodeLoaded decoder =
  Maybe.andThen (\string ->
    string
     |> Json.Decode.decodeString decoder
     |> Result.mapError (Debug.log "local storage decode error")
     |> Result.toMaybe
    )

port localStorageSave : String -> Cmd msg
port localStorageLoaded : (Maybe String -> msg) -> Sub msg
