module Main

open Suave.Filters
open Suave.Operators
open Suave.Successful
open Feliz.ViewEngine
open Feliz.ViewEngine.Htmx

(*
When a user clicks on this button, 
issue an HTTP GET request to ‘/clicked’ 
and use the content from the response 
to replace the element with the id parent-div in the DOM
*)

let body =
    [ Html.h1 "HTMX is COOL"
      Html.button
          [ hx.get "/clicked"
            hx.swap.outerHTML
            hx.trigger "click"
            hx.target "#result"
            prop.text "HTTP GET TO SERVER HTML RESPONSE" ]
      Html.div [ prop.id "result" ] ]

let mainLayout =
    Html.html
        [ Html.head
              [ Html.title "F# ♥ Htmx"
                Html.script [ prop.src "https://unpkg.com/htmx.org@1.6.0" ]
                Html.meta [ prop.charset.utf8 ] ]
          Html.body body ]
    |> Render.htmlView

let mainApp = path "/main" >=> OK mainLayout

let ssr =
    Html.ul [ Html.li "H"; Html.li "T"; Html.li "M"; Html.li "X" ]
    |> Render.htmlView

let getResponseApp = path "/clicked" >=> OK ssr
