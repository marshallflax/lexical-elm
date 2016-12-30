module Misc exposing (..)

import Set exposing (Set)

toggleSet : comparable1 -> Set comparable1 -> Set comparable1
toggleSet element set =
    if (Set.member element set) then
        Set.remove element set
    else
        Set.insert element set
