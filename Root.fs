module Root

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

let (|Plain|Htmx|) ctx =
    let hxRequest = ctx.request.headers |> List.tryFind (fun (k, _) -> k = "hx-request")
    if hxRequest.IsSome then Htmx else Plain

let rootAppTemplate detail =
    async {
        let! contacts = Data.getContacts ()

        let sidebar = Views.sidebarElements None (Views.navElement contacts)
        return Views.appView sidebar detail
    }

let rootApp: WebPart =
    path "/"
    >=> fun ctx ->
        async {
            let q = ctx.request.queryParamOpt "q"

            let query =
                match q with
                | Some(k, v) -> v
                | None -> None

            let! contacts =
                match query with
                | Some q -> Data.queryContacts q
                | None -> Data.getContacts ()

            let nav = Views.navElement contacts

            match ctx with
            | Htmx ->
                let view = Views.partialView nav
                return! OK view ctx
            | Plain ->
                let view = Views.appView (Views.sidebarElements query nav) Views.emptyDetailElement
                return! OK view ctx
        }

let createContactApp: WebPart =
    path "/"
    >=> fun ctx ->
        async {
            let! contact = Data.createEmptyContact ()
            // return! Redirection.see_other $"/contacts/{contact.id}/edit" ctx
            let location = ("Location", $"/contacts/{contact.id}")
            let hxRedirect = ("HX-Redirect", $"/contacts/{contact.id}/edit")
            let newHeaders = location :: hxRedirect :: ctx.response.headers

            let newCtx =
                { ctx with
                    response =
                        { ctx.response with
                            headers = newHeaders } }

            return! CREATED "" newCtx
        }

let swapNav =
    async {
        let! contacts = Data.getContacts ()
        return Views.navElement contacts
    }

let swapSidebar (query : string option) =
    async {
        let searchForm = Views.searchFormElement query
        let! contacts =
                    match query with
                    | Some q -> Data.queryContacts q
                    | None -> Data.getContacts ()
        let nav = Views.navElement contacts
        return [ searchForm; nav]
    }
