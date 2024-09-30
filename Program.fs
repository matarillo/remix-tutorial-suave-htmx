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
                  [ Root.rootApp
                    Main.mainApp
                    Main.getResponseApp
                    Contact.getEditContactApp
                    Contact.getContactApp
                    browseHome ]
          POST
          >=> choose
                  [ Root.createContactApp
                    Contact.editContactApp
                    Contact.destroyContactApp
                    Contact.htmxToggleFavoriteApp
                    RequestErrors.METHOD_NOT_ALLOWED (Views.defaultError "405 Method Not Allowed") ]
          RequestErrors.METHOD_NOT_ALLOWED (Views.defaultError "405 Method Not Allowed") ]

let config =
    { defaultConfig with
        homeFolder = Some(Path.GetFullPath "./public") }

startWebServer config app
