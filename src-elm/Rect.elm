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


leftmostRectangle : List Rectangle -> Maybe Rectangle
leftmostRectangle rects =
    case rects of
        [] ->
            Nothing

        rect :: rest ->
            let
                leftmost =
                    List.foldl
                        (\a b ->
                            if a.x1 < b.x1 then
                                a

                            else
                                b
                        )
                        rect
                        rest
            in
            Just leftmost


rightmostRectangle : List Rectangle -> Maybe Rectangle
rightmostRectangle rects =
    case rects of
        [] ->
            Nothing

        rect :: rest ->
            let
                rightmost =
                    List.foldl
                        (\a b ->
                            if a.x1 + a.width > b.x1 + b.width then
                                a

                            else
                                b
                        )
                        rect
                        rest
            in
            Just rightmost


topmostRectangle : List Rectangle -> Maybe Rectangle
topmostRectangle rects =
    case rects of
        [] ->
            Nothing

        rect :: rest ->
            let
                topmost =
                    List.foldl
                        (\a b ->
                            if a.y1 < b.y1 then
                                a

                            else
                                b
                        )
                        rect
                        rest
            in
            Just topmost


bottommostRectangle : List Rectangle -> Maybe Rectangle
bottommostRectangle rects =
    case rects of
        [] ->
            Nothing

        rect :: rest ->
            let
                bottommost =
                    List.foldl
                        (\a b ->
                            if a.y1 + a.height > b.y1 + b.height then
                                a

                            else
                                b
                        )
                        rect
                        rest
            in
            Just bottommost
