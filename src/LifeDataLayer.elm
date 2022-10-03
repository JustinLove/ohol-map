module LifeDataLayer exposing
  ( LifeDataLayer
  , empty
  , load
  , withLives
  , failed
  , hasData
  , canMakeRequest
  )

import OHOLData as Data
import RemoteData exposing (RemoteData(..))

import Http

type alias LifeDataLayer =
  { serverId : Int
  , lives : RemoteData Int
  }

empty : LifeDataLayer
empty =
  { serverId = 0
  , lives = NotRequested
  }

load : Int -> LifeDataLayer
load server =
  { serverId = server
  , lives = Loading
  }

withLives : Int -> List Data.Life -> LifeDataLayer
withLives server lives =
  { serverId = server
  , lives = Data server
  }

failed : Int -> Http.Error -> LifeDataLayer
failed server error =
  { serverId = server
  , lives = Failed error
  }

hasData : LifeDataLayer -> Bool
hasData data =
  data.lives /= NotRequested

canMakeRequest : LifeDataLayer -> Bool
canMakeRequest data =
  case data.lives of
    NotRequested -> True
    NotAvailable -> False
    Loading -> False
    Data _ -> True
    Failed _ -> True
