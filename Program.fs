open Suave
open Suave.Filters
open Suave.Operators
open Suave.Files

open System.IO

Data.init ()

let app =
    choose
        [ GET
          >=> choose
                  [ path "/" >=> Root.rootApp
                    path "/main" >=> Main.mainApp
                    path "/clicked" >=> Main.getResponseApp
                    pathScan "/contacts/%s/edit" Contact.getEditContactApp
                    pathScan "/contacts/%s" Contact.getContactApp
                    browseHome ]
          POST
          >=> choose
                  [ path "/" >=> Root.createContactApp
                    pathScan "/contacts/%s/edit" Contact.editContactApp
                    pathScan "/contacts/%s/destroy" Contact.destroyContactApp
                    pathScan "/contacts/%s" Contact.htmxToggleFavoriteApp
                    RequestErrors.METHOD_NOT_ALLOWED(Views.defaultError "405 Method Not Allowed") ]
          RequestErrors.METHOD_NOT_ALLOWED(Views.defaultError "405 Method Not Allowed") ]

let config =
    { defaultConfig with
        homeFolder = Some(Path.GetFullPath "./public") }

startWebServer config app
