module Root

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

let rootAppTemplate detail =
    async {
        let! contacts = Data.getContacts ()
        let sidebar = Views.sidebarElements (Views.navElement contacts)
        return Views.appView sidebar detail
    }

let rootApp: WebPart =
    path "/"
    >=> fun ctx ->
        async {
            let! view = rootAppTemplate Views.emptyDetailElement
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
