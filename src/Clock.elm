module Clock exposing (viewClock)

import Svg exposing (..)
import Svg.Attributes exposing (..)


viewClock : Int -> Int -> Int -> Svg msg
viewClock hour minute second =
    svg
        [ viewBox "-10 -10 20 20" ]
        [ secondHand second, minuteHand minute second, hourHand hour minute second ]


secondHand second =
    hand 10.0 0.3 (toFloat second * 6)


minuteHand minute second =
    let
        minF =
            toFloat minute

        secF =
            toFloat second
    in
    hand 9.0 0.7 ((minF + secF / 60) * 6)


hourHand hour minute second =
    let
        hF =
            toFloat hour

        minF =
            toFloat minute

        secF =
            toFloat second
    in
    hand 8.0 1.0 ((hF + (minF + secF / 60) / 60) * 30)


hand : Float -> Float -> Float -> Svg msg
hand len w deg =
    let
        halfWidth =
            String.fromFloat (w / 2)

        negHalfWidth =
            String.fromFloat (-w / 2)
    in
    rect
        [ x negHalfWidth
        , y negHalfWidth
        , width (String.fromFloat w)
        , height (String.fromFloat len)
        , rx halfWidth
        , ry halfWidth
        , transform (transformRotate (deg + 180))
        ]
        []


transformRotate : Float -> String
transformRotate deg =
    "rotate(" ++ String.fromFloat deg ++ ")"
