module Main

open Feliz
open Browser.Dom
open Fable.Core.JsInterop

importSideEffects "./styles/global.scss"

ReactDOM.render (Labyrinf.Components.Faze(), document.getElementById "feliz-app")

let x = [ 1; 2; 3 ]
