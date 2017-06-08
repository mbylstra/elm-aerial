module Aerial.Styles exposing (..)


styles : String
styles =
    """
.tile-layer__fade-in {
    animation: fade-in 1s;
}

@keyframes fade-in {
    from { opacity: 0; }
    to   { opacity: 1; }
}

/* Firefox < 16 */
@-moz-keyframes fade-in {
    from { opacity: 0; }
    to   { opacity: 1; }
}

/* Safari, Chrome and Opera > 12.1 */
@-webkit-keyframes fade-in {
    from { opacity: 0; }
    to   { opacity: 1; }
}

/* Internet Explorer */
@-ms-keyframes fade-in {
    from { opacity: 0; }
    to   { opacity: 1; }
}

/* Opera < 12.1 */
@-o-keyframes fade-in {
    from { opacity: 0; }
    to   { opacity: 1; }
}
"""
