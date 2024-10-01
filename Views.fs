module Views

open Feliz.ViewEngine
open Feliz.ViewEngine.Htmx

let inline fragment elements = React.fragment elements

let defaultError (message: string) =
    "<!DOCTYPE html>"
    + Render.htmlView (Html.html [ Html.head [ Html.title "Error" ]; Html.body [ Html.h1 message ] ])

let inline private hasValue s = not (String.isEmpty s)

let favButton (contact: Data.ContactRecord) =
    let fav = contact.favorite

    Html.button
        [ prop.ariaLabel (if fav then "Remove from favorites" else "Add to favorite")
          prop.name "favorite"
          prop.value (if fav then "false" else "true")
          prop.text (if fav then "★" else "☆") ]

let contactElement (contact: Data.ContactRecord) =
    Html.div
        [ prop.id "contact"
          prop.children
              [ Html.div
                    [ Html.img
                          [ prop.alt $"{contact.first} {contact.last} avatar"
                            prop.key contact.avatar
                            prop.src contact.avatar ] ]
                Html.div
                    [ Html.h1
                          [ if hasValue contact.first || hasValue contact.last then
                                Html.text $"{contact.first} {contact.last}"
                            else
                                Html.i "No Name"
                            Html.text " "
                            Html.form [ hx.post ""; prop.children [ favButton contact ] ] ]
                      if hasValue contact.twitter then
                          Html.p
                              [ Html.a
                                    [ prop.href $"https://twitter.com/{contact.twitter}"
                                      prop.text contact.twitter ] ]
                      else
                          Html.none
                      if hasValue contact.notes then
                          Html.p contact.notes
                      else
                          Html.none
                      Html.div
                          [ Html.form
                                [ hx.get $"/contacts/{contact.id}/edit"
                                  hx.target "#detail"
                                  hx.pushUrl true
                                  prop.children [ Html.button [ prop.type' "submit"; prop.text "Edit" ] ] ]
                            Html.form
                                [ prop.action "destroy"
                                  hx.post $"/contacts/{contact.id}/destroy"
                                  hx.confirm "Please confirm you want to delete this record."
                                  prop.children [ Html.button [ prop.type' "submit"; prop.text "Delete" ] ] ] ] ] ] ]

let contactPartialView = contactElement >> Render.htmlView

let emptyDetailElement =
    Html.p
        [ prop.id "index-page"
          prop.children
              [ Html.text "This is a demo for Remix."
                Html.br []
                Html.text "Check out "
                Html.a [ prop.href "https://remix.run"; prop.text "the docs at remix.run" ]
                Html.text "." ] ]

let searchFormElement (query: string option) =
    let inputValue = Option.defaultValue "" query

    Html.form
        [ prop.id "search-form"
          prop.role "search"
          prop.custom ("data-discover", true)
          hx.get "/"
          hx.target "#contacts"
          hx.pushUrl true
          hx.indicator "#q"
          hx.swapOob "true"
          prop.children
              [ Html.input
                    [ prop.id "q"
                      prop.name "q"
                      prop.type' "search"
                      hx.get "/"
                      hx.target "#contacts"
                      hx.pushUrl true
                      hx.trigger "input changed delay:500ms, search"
                      prop.placeholder "Search"
                      prop.ariaLabel "Search contacts"
                      prop.value inputValue ]
                Html.div [ prop.id "search-spinner"; prop.ariaHidden true; prop.className "search-spinner" ] ] ]

let notFoundDetailElement = Html.h1 "Not Found"

let htmxLink target (contact: Data.ContactRecord) =
    Html.a
        [ prop.href "#"
          hx.get $"/contacts/{contact.id}"
          hx.target target
          hx.pushUrl true
          prop.children
              [ if hasValue contact.first || hasValue contact.last then
                    Html.text $"{contact.first} {contact.last}"
                else
                    Html.i "No Name"
                Html.text " "
                if contact.favorite then Html.span "★" else Html.none ] ]

let navListItem (isOobResponse: bool) (contact: Data.ContactRecord) =
    let props = [ prop.key contact.id; prop.children [ htmxLink "#detail" contact ] ]

    if isOobResponse then
        let swapOob = hx.swapOob $"""true:ul>li[key="{contact.id}"]"""
        Html.li (swapOob :: props)
    else
        Html.li props

let navElement (contacts: Data.ContactRecord list) =
    Html.nav
        [ prop.id "contacts"
          hx.swapOob "true"
          prop.children
              [ match contacts with
                | _ :: _ -> Html.ul [ for contact in contacts -> navListItem false contact ]
                | _ -> Html.p [ Html.i "No contacts" ] ] ]

let sidebarElements (query : string option) (nav: ReactElement) =
    fragment
        [ Html.h1 [ prop.text "Remix Contacts" ]
          Html.div
              [ searchFormElement query
                Html.form
                    [ prop.action "/"
                      hx.post "/"
                      prop.children [ Html.button [ prop.type' "submit"; prop.text "New" ] ] ] ]
          nav ]

let appStylesHref = "/css/app.css?url"

let appElement (sidebar: ReactElement) (detail: ReactElement) =
    Html.html
        [ prop.lang "en"
          prop.children
              [ Html.head
                    [ Html.meta [ prop.charset "utf-8" ]
                      Html.meta [ prop.name "viewport"; prop.value "width=device-width, initial-scale=1" ]
                      Html.script [ prop.src "https://unpkg.com/htmx.org@1.6.0" ]
                      Html.link [ prop.rel "stylesheet"; prop.href appStylesHref ] ]
                Html.body
                    [ Html.div [ prop.id "sidebar"; prop.children sidebar ]
                      Html.div [ prop.id "detail"; prop.children detail ] ] ] ]

let appView sidebar detail =
    let html = appElement sidebar detail |> Render.htmlView
    "<!DOCTYPE html>" + html

let partialView (element: ReactElement) = Render.htmlView element

let editContactElement (contact: Data.ContactRecord) =
    Html.form
        [ prop.key contact.id
          prop.id "contact-form"
          hx.post $"/contacts/{contact.id}/edit"
          hx.target "#detail"
          prop.custom ("hx-push-url", $"/contacts/{contact.id}")
          prop.children
              [ Html.p
                    [ Html.span "Name"
                      Html.input
                          [ prop.value contact.first
                            prop.ariaLabel "First name"
                            prop.name "first"
                            prop.type' "text"
                            prop.placeholder "First" ]
                      Html.input
                          [ prop.value contact.last
                            prop.ariaLabel "Last name"
                            prop.name "last"
                            prop.type' "text"
                            prop.placeholder "Lirst" ] ]
                Html.label
                    [ Html.span "Twitter"
                      Html.input
                          [ prop.value contact.twitter
                            prop.name "twitter"
                            prop.placeholder "@jack"
                            prop.type' "text" ] ]
                Html.label
                    [ Html.span "Avatar URL"
                      Html.input
                          [ prop.value contact.avatar
                            prop.ariaLabel "Avatar URL"
                            prop.name "avatar"
                            prop.placeholder "https://example.com/avatar.jpg"
                            prop.type' "text" ] ]
                Html.label
                    [ Html.span "Notes"
                      Html.textarea [ prop.value contact.notes; prop.name "notes"; prop.rows 6 ] ]
                Html.p
                    [ Html.button [ prop.type' "submit"; prop.text "Save" ]
                      Html.button
                          [ prop.type' "button"
                            prop.id "cancel-contact-form"
                            prop.text "Cancel"
                            hx.get $"/contacts/{contact.id}"
                            hx.target "#detail" ] ] ] ]
