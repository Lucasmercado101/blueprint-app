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
