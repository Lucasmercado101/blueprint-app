module Point exposing (..)


x : ( number, number ) -> number
x ( x1, y1 ) =
    x1


y : ( number, number ) -> number
y ( x1, y1 ) =
    y1


add : ( number, number ) -> ( number, number ) -> ( number, number )
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


subtract : ( number, number ) -> ( number, number ) -> ( number, number )
subtract ( x1, y1 ) ( x2, y2 ) =
    ( x1 - x2, y1 - y2 )


scale : number -> ( number, number ) -> ( number, number )
scale factor ( x1, y1 ) =
    ( x1 * factor, y1 * factor )
