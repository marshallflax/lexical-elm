module MainView exposing (viewSubscriptions, root)

import BowlingScoreView
import Html exposing (Html, button, div, span, text, input, p, table, tr, td)
import Html.Attributes exposing (href)
import Html.Lazy exposing (lazy, lazy2)
import LexicalView
import Styles
import Types exposing (Model, Msg(KeyMsg))


root : Model -> Html Msg
root model =
    div []
        [ Styles.defs
        , lazy LexicalView.viewLexicalModel ( model.draggables, model.lexical )
        , colophon
        , table []
            [ tr []
                [ td [] [ lazy BowlingScoreView.showTestResultsOld model.bowlingResults ]
                , td [] [ lazy2 BowlingScoreView.showTestResults model.tableState model.bowlingResults ]
                ]
            ]
        , p [ Styles.useClass Styles.DisplayNone ] [ text (toString model.lastKeyCode) ]
        ]


{-| Or, Keyboard.downs KeyMsg
-}
viewSubscriptions : Model -> Sub Msg
viewSubscriptions _ =
    Sub.none


colophon : Html msg
colophon =
    p [ Styles.useClass Styles.Colophon ]
        [ text "(c) marshall.flax@gmail.com; licensed "
        , Html.a [ href "https://www.gnu.org/licenses/gpl-3.0.en.html" ] [ text "GPL3.0 +" ]
        , text " "
        , Html.a [ href "https://github.com/marshallflax/lexical-elm" ] [ text "source" ]
        , text " "
        , Html.a [ href "https://raw.githubusercontent.com/marshallflax/lexical-elm/master/index.html" ] [ text "latest" ]
        ]
