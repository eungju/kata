USING: cairo cairo.ffi colors combinators kernel math math.rectangles
prettyprint random tools.time system ;

[
100 [
    { 256 256 } [
        {
            [ CAIRO_ANTIALIAS_DEFAULT cairo_set_antialias ]
            [ 1 1 1 1 <rgba> set-source-color ]
            [ { 0 0 } { 255 255 } <rect> fill-rect ]
            [ 1 0 0 1 <rgba> set-source-color ]
            [ 1 cairo_set_line_width ]
            [ 50 [
                dup
                {
                    [ 256 random 256 random cairo_move_to ]
                    [ 49 [ dup 256 random 256 random cairo_line_to ] times drop ]
                    [ cairo_stroke ]
                } cleave
            ] times drop
            ]
        } cleave ] make-bitmap-image drop
] times
] time
