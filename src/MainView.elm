module MainView exposing (viewSubscriptions, root)

import BowlingScoreView
import Css
import DragView exposing (viewDraggables)
import Dict
import Html exposing (Html, button, div, span, text, input, p, table, tr, td)
import Html.Attributes exposing (style, value, checked, type_, readonly, placeholder, href)
import Html.Lazy exposing (lazy, lazy2)
import Keyboard
import LexicalView
import Types exposing (Model, Msg(KeyMsg))


root : Model -> Html Msg
root model =
    div []
        [ Css.style [ Html.Attributes.scoped True ] LexicalView.stylesheet
        , lazy DragView.viewDraggables (Dict.toList model.draggables)
        , lazy LexicalView.viewLexicalModel model.lexical
        , colophon
        , table []
            [ tr []
                [ td [] [ lazy BowlingScoreView.showTestResultsOld model.bowlingResults ]
                , td [] [ lazy2 BowlingScoreView.showTestResults model.tableState model.bowlingResults ]
                ]
            ]
        , p [] [ text (toString model.lastKeyCode) ]
        ]


viewSubscriptions : Model -> Sub Msg
viewSubscriptions _ =
    Keyboard.downs KeyMsg


colophon : Html msg
colophon =
    p [ style [ ( "fontSize", "25%" ) ] ]
        [ text "(c) marshall.flax@gmail.com; licensed "
        , Html.a [ href "https://www.gnu.org/licenses/gpl-3.0.en.html" ] [ text "GPL3.0 +" ]
        , text " "
        , Html.a [ href "https://github.com/marshallflax/lexical-elm" ] [ text "source" ]
        , text " "
        , Html.a [ href "https://raw.githubusercontent.com/marshallflax/lexical-elm/master/index.html" ] [ text "latest" ]
        ]
