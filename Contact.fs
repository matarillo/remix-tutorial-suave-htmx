module Contact

open Suave
open Suave.Operators
open Suave.Filters
open Suave.Successful
open Suave.RequestErrors

let (|Plain|Htmx|) ctx =
    let hxRequest = ctx.request.headers |> List.tryFind (fun (k, _) -> k = "hx-request")
    if hxRequest.IsSome then Htmx else Plain

let htmxAppTemplate contactElement ctx =
    async {
        match ctx with
        | Htmx ->
            let! oobElements = Root.swapSidebar None
            let resultElements = Views.fragment (contactElement :: oobElements)
            let view = Views.partialView resultElements
            return! OK view ctx
        | Plain ->
            let! view = Root.rootAppTemplate contactElement
            return! OK view ctx
    }

let getContactApp: WebPart =
    pathScan "/contacts/%s" (fun id ctx ->
        async {
            let! contact = Data.getContact id

            let contactElement =
                match contact with
                | Ok c -> Views.contactElement c
                | Error msg -> Views.notFoundDetailElement

            return! htmxAppTemplate contactElement ctx
        })

let htmxToggleFavoriteApp: WebPart =
    pathScan "/contacts/%s" (fun id ->
        request (fun req ->
            match Data.ContactMutation.FromFormData req.form with
            | None -> BAD_REQUEST "Invalid Form Data"
            | Some updates ->
                fun ctx ->
                    async {
                        let! contact = Data.getContact id

                        match contact with
                        | Error msg -> return! NOT_FOUND msg ctx
                        | Ok _ ->
                            let! result = Data.updateContact id updates

                            match result with
                            | Error msg -> return! BAD_REQUEST msg ctx
                            | Ok c ->
                                let favButton = Views.favButton c
                                let navListItem = Views.navListItem true c
                                let fragment = Views.fragment [ favButton; navListItem ]
                                let view = Views.partialView fragment
                                return! OK view ctx
                    }))

let getEditContactApp: WebPart =
    pathScan "/contacts/%s/edit" (fun id ctx ->
        async {
            let! contact = Data.getContact id

            let contactElement =
                match contact with
                | Ok c -> Views.editContactElement c
                | Error msg -> Views.notFoundDetailElement

            return! htmxAppTemplate contactElement ctx
        })

let editContactApp =
    pathScan "/contacts/%s/edit" (fun id ->
        request (fun req ->
            match Data.ContactMutation.FromFormData req.form with
            | None -> BAD_REQUEST "Invalid Form Data"
            | Some updates ->
                fun ctx ->
                    async {
                        let! contact = Data.getContact id

                        match contact with
                        | Error msg -> return! NOT_FOUND msg ctx
                        | Ok _ ->
                            let! result = Data.updateContact id updates

                            match result with
                            | Error msg -> return! BAD_REQUEST msg ctx
                            | Ok c ->
                                let contactElement = Views.contactElement c
                                let! oobElement = Root.swapNav // Re-sort the list as a result of the name change
                                let fragment = Views.fragment [ contactElement; oobElement ]
                                let view = Views.partialView fragment
                                return! OK view ctx
                    }))

let destroyContactApp =
    pathScan "/contacts/%s/destroy" (fun id ->
        fun ctx ->
            async {
                do! Data.deleteContact id
                // return! Redirection.see_other "/" ctx
                let hxRedirect = ("HX-Redirect", "/")
                let newHeaders = hxRedirect :: ctx.response.headers

                let newCtx =
                    { ctx with
                        response =
                            { ctx.response with
                                headers = newHeaders } }

                return! OK "" newCtx // HTMX doesn't support 204 No Content
            })
