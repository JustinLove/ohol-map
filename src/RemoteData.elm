module RemoteData exposing (RemoteData(..), withDefault)

import Http

type RemoteData a
  = NotRequested
  | Loading
  | Data a
  | Failed Http.Error

withDefault : a -> RemoteData a -> a
withDefault default data =
  case data of
    Data value -> value
    _ -> default
