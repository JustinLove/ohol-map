module RemoteData exposing (RemoteData(..))

import Http

type RemoteData a
  = NotRequested
  | Loading
  | Data a
  | Failed Http.Error
