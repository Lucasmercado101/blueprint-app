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


xAs1DLine : Rectangle -> ( Int, Int )
xAs1DLine rect =
    ( rect.x1, rect.x1 + rect.width )


bottomY : Rectangle -> Int
bottomY rect =
    rect.y1 + rect.height


rightX : Rectangle -> Int
rightX rect =
    rect.x1 + rect.width


isPointOnRectangle : Point -> Rectangle -> Bool
isPointOnRectangle ( x, y ) rect =
    let
        ( x1, y1 ) =
            topLeft rect

        ( x2, y2 ) =
            bottomRight rect
    in
    x >= x1 && x <= x2 && y >= y1 && y <= y2


rectanglesPointIsOn : Point -> List Rectangle -> List Rectangle
rectanglesPointIsOn p rects =
    List.filter (isPointOnRectangle p) rects


center : Rectangle -> Point
center rect =
    ( rect.x1 + rect.width // 2, rect.y1 + rect.height // 2 )


centerY : Rectangle -> Int
centerY rect =
    rect.y1 + (rect.height // 2)


centerX : Rectangle -> Int
centerX rect =
    rect.x1 + (rect.width // 2)


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


subPosition : Point -> Rectangle -> Rectangle
subPosition ( x, y ) rect =
    { rect
        | x1 = rect.x1 - x
        , y1 = rect.y1 - y
    }


addPosition : Point -> Rectangle -> Rectangle
addPosition ( x, y ) rect =
    { rect
        | x1 = rect.x1 + x
        , y1 = rect.y1 + y
    }


eq : Rectangle -> Rectangle -> Bool
eq rect1 rect2 =
    rect1.x1 == rect2.x1 && rect1.y1 == rect2.y1 && rect1.width == rect2.width && rect1.height == rect2.height


isThereAnyOverlap : Rectangle -> Rectangle -> Bool
isThereAnyOverlap firstRectangle secondRectangle =
    let
        x1 =
            firstRectangle.x1

        y1 =
            firstRectangle.y1

        width1 =
            firstRectangle.width

        height1 =
            firstRectangle.height

        x2 =
            secondRectangle.x1

        y2 =
            secondRectangle.y1

        width2 =
            secondRectangle.width

        height2 =
            secondRectangle.height
    in
    -- top left is between the start and end
    (x1 >= x2 && x1 <= x2 + width2 && y1 >= y2 && y1 <= y2 + height2)
        -- top right is between the start and end
        || (x1 + width1 >= x2 && x1 + width1 <= x2 + width2 && y1 >= y2 && y1 <= y2 + height2)
        -- bottom left is between the start and end
        || (x1 >= x2 && x1 <= x2 + width2 && y1 + height1 >= y2 && y1 + height1 <= y2 + height2)
        -- bottom right is between the start and end
        || (x1 + width1 >= x2 && x1 + width1 <= x2 + width2 && y1 + height1 >= y2 && y1 + height1 <= y2 + height2)
        -- same but with the other rectangle
        || (x2 >= x1 && x2 <= x1 + width1 && y2 >= y1 && y2 <= y1 + height1)
        || (x2 + width2 >= x1 && x2 + width2 <= x1 + width1 && y2 >= y1 && y2 <= y1 + height1)
        || (x2 >= x1 && x2 <= x1 + width1 && y2 + height2 >= y1 && y2 + height2 <= y1 + height1)
        || (x2 + width2 >= x1 && x2 + width2 <= x1 + width1 && y2 + height2 >= y1 && y2 + height2 <= y1 + height1)
        -- TODO: this might be unnecesary, recheck math
        -- rectangle A is inside of rectangle B
        || (y2 >= y1 && y2 + height2 <= y1 + height1 && x2 <= x1 && x2 + width2 >= x1 + width1)
        -- rectangle B is inside of rectangle A
        || (y1 >= y2 && y1 + height1 <= y2 + height2 && x1 <= x2 && x1 + width1 >= x2 + width2)



{- is the second rectangle inside the first? -}


isInside : Rectangle -> Rectangle -> Bool
isInside firstRectangle secondRectangle =
    let
        x1 =
            firstRectangle.x1

        y1 =
            firstRectangle.y1

        width1 =
            firstRectangle.width

        height1 =
            firstRectangle.height

        x2 =
            secondRectangle.x1

        y2 =
            secondRectangle.y1

        width2 =
            secondRectangle.width

        height2 =
            secondRectangle.height
    in
    y2 >= y1 && y2 + height2 <= y1 + height1 && x2 >= x1 && x2 + width2 <= x1 + width1


closestRectangleX : Rectangle -> List Rectangle -> Maybe Rectangle
closestRectangleX rect rects =
    case rects of
        [] ->
            Nothing

        [ onlyRect ] ->
            Just onlyRect

        x :: xs ->
            Just
                (List.foldl
                    (\next current ->
                        let
                            centerXCurrent =
                                current.x1 + (current.width // 2)

                            centerXNext =
                                next.x1 + (next.width // 2)

                            dNext =
                                abs (centerX rect - centerXNext)

                            dCurr =
                                abs (centerX rect - centerXCurrent)
                        in
                        if dNext < dCurr then
                            next

                        else
                            current
                    )
                    x
                    xs
                )


closestRectangleY : Rectangle -> List Rectangle -> Maybe Rectangle
closestRectangleY rect rects =
    case rects of
        [] ->
            Nothing

        [ onlyRect ] ->
            Just onlyRect

        x :: xs ->
            Just
                (List.foldl
                    (\next current ->
                        let
                            centerYCurrent =
                                current.y1 + (current.height // 2)

                            centerYNext =
                                next.y1 + (next.height // 2)

                            dNext =
                                abs (centerY rect - centerYNext)

                            dCurr =
                                abs (centerY rect - centerYCurrent)
                        in
                        if dNext < dCurr then
                            next

                        else
                            current
                    )
                    x
                    xs
                )


changePosition : Rectangle -> Point -> Rectangle
changePosition rect ( x, y ) =
    { rect | x1 = x, y1 = y }
