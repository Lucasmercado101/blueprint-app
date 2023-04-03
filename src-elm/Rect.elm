module Rect exposing (..)


type alias Point =
    ( Int, Int )


type alias Rectangle =
    { x1 : Int, y1 : Int, width : Int, height : Int }


bottomLeft : Rectangle -> Point
bottomLeft rect =
    ( rect.x1, rect.y1 + rect.height )


bottomRight : Rectangle -> Point
bottomRight rect =
    ( rect.x1 + rect.width, rect.y1 + rect.height )


topLeft : Rectangle -> Point
topLeft rect =
    ( rect.x1, rect.y1 )


topRight : Rectangle -> Point
topRight rect =
    ( rect.x1 + rect.width, rect.y1 )


bottomSide : Rectangle -> ( Point, Point )
bottomSide rect =
    ( bottomLeft rect, bottomRight rect )


bottomY : Rectangle -> Int
bottomY rect =
    rect.y1 + rect.height


x : Point -> Int
x p =
    Tuple.first p


y : Point -> Int
y p =
    Tuple.second p


isOnRectangle : Point -> Rectangle -> Bool
isOnRectangle p rect =
    let
        ( x1, y1 ) =
            topLeft rect

        ( x2, y2 ) =
            bottomRight rect
    in
    x p >= x1 && x p <= x2 && y p >= y1 && y p <= y2


center : Rectangle -> Point
center rect =
    ( rect.x1 + rect.width // 2, rect.y1 + rect.height // 2 )
