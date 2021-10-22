module Clock exposing (viewClock)

import Svg exposing (..)
import Svg.Attributes exposing (..)


viewClock : Int -> Int -> Int -> Svg msg
viewClock hour minute second =
    svg
        [ viewBox "-10 -10 20 20" ]
        [ ticks, secondHand second, minuteHand minute second, hourHand hour minute second ]


ticks : Svg msg
ticks =
    g [] (List.map tick (List.range 0 59))


tick : Int -> Svg msg
tick minute =
    let
        size =
            if remainderBy 5 minute == 0 then
                "0.4"

            else
                "0.1"
    in
    circle [ transform (transformRotate (toFloat minute * 6)), cx "0", cy "-9", r size ] []


secondHand second =
    hand 8.5 0.3 (toFloat second * 6)


minuteHand minute second =
    let
        minF =
            toFloat minute

        secF =
            toFloat second
    in
    hand 8.0 0.7 ((minF + secF / 60) * 6)


hourHand hour minute second =
    let
        hF =
            toFloat hour

        minF =
            toFloat minute

        secF =
            toFloat second
    in
    hand 7.0 0.9 ((hF + (minF + secF / 60) / 60) * 30)


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
