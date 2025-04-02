module Contact

open Feliz.ViewEngine
open Suave
open Suave.Successful
open Suave.RequestErrors

let (|Plain|Htmx|) ctx =
    let hxRequest = ctx.request.headers |> List.tryFind (fun (k, _) -> k = "hx-request")
    if hxRequest.IsSome then Htmx else Plain

let htmxAppTemplate (selectedId: string option) (contactElement: ReactElement) : WebPart =
    fun ctx ->
        async {
            match ctx with
            | Htmx ->
                let! oobElements = Root.swapSidebar selectedId None
                let resultElements = Views.fragment (contactElement :: oobElements)
                let view = Views.partialView resultElements
                return! OK view ctx
            | Plain ->
                let! view = Root.rootAppTemplate selectedId contactElement
                return! OK view ctx
        }

let getContactApp (id: string) : WebPart =
    fun ctx ->
        async {
            let! contact = Data.getContact id

            let contactElement =
                match contact with
                | Ok c -> Views.contactElement c
                | Error msg -> Views.notFoundDetailElement

            let selectedId =
                match contact with
                | Ok c -> Some c.id
                | _ -> None

            return! htmxAppTemplate selectedId contactElement ctx
        }

let htmxToggleFavoriteApp (id: string) : WebPart =
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
                            let navListItem = Views.navListItem true true c // selected and updated
                            let fragment = Views.fragment [ favButton; navListItem ]
                            let view = Views.partialView fragment
                            return! OK view ctx
                })

let getEditContactApp (id: string) : WebPart =
    fun ctx ->
        async {
            let! contact = Data.getContact id

            let contactElement =
                match contact with
                | Ok c -> Views.editContactElement c
                | Error msg -> Views.notFoundDetailElement

            let selectedId =
                match contact with
                | Ok c -> Some c.id
                | _ -> None

            return! htmxAppTemplate selectedId contactElement ctx
        }

let editContactApp (id: string) : WebPart =
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
                            let! oobElement = Root.swapNav (Some c.id) // Re-sort the list as a result of the name change
                            let fragment = Views.fragment [ contactElement; oobElement ]
                            let view = Views.partialView fragment
                            return! OK view ctx
                })

let destroyContactApp (id: string) : WebPart =
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
        }
