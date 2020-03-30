module OHOLDataDecodeTest exposing (..)

import OHOLData as Data
import OHOLData.Decode as Decode

import Time exposing (Posix)
import Json.Decode as Decode

import Expect exposing (Expectation)
import Test exposing (..)

gp = """
{
  "id": "3888",
  "mapChance": 0.02,
  "biomes": [
    4
  ],
  "gridPlacement": 20,
  "gridPlacementY": 40,
  "gridPlacementPhaseX": 3
}
"""

suite : Test
suite =
  describe "grid placement"
    [ test "phase" <| \_ ->
      (Decode.decodeString Decode.gridPlacement gp)
        |> Expect.equal (Ok (Data.GridPlacement 20 40 3 0))
    ]
