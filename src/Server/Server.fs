module Server

open Falco
open Falco.Routing
open Falco.HostBuilder
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection

//let todosApi =
//    { getTodos = fun () -> async { return storage.GetTodos() }
//      addTodo =
//        fun todo -> async {
//            match storage.AddTodo todo with
//            | Ok () -> return todo
//            | Error e -> return failwith e
//        } }

//let webApp =
//    Remoting.createApi()
//    |> Remoting.withRouteBuilder Route.builder
//    |> Remoting.fromValue todosApi
//    |> Remoting.buildHttpHandler


let configureServices (services : IServiceCollection) =
    services.AddFalco() |> ignore

// ------------
// Activate middleware
// ------------
let configureApp (endpoints : HttpEndpoint list) (ctx : WebHostBuilderContext) (app : IApplicationBuilder) =    
    let devMode = StringUtils.strEquals ctx.HostingEnvironment.EnvironmentName "Development"    
    app.UseWhen(devMode, fun app -> 
            app.UseDeveloperExceptionPage())
       .UseWhen(not(devMode), fun app -> 
            app.UseFalcoExceptionHandler(Response.withStatusCode 500 >> Response.ofPlainText "Server error"))
       .UseFalco(endpoints) |> ignore
       
// -----------
// Configure Host
// -----------
let configureHost (endpoints : HttpEndpoint list) (webhost : IWebHostBuilder) =
    webhost.ConfigureServices(configureServices)
           .Configure(configureApp endpoints)
           .UseUrls([| "http://0.0.0.0:8085" |])


webHost [||] {
    configure configureHost
    endpoints [                    
        get "/api/" (Response.ofPlainText "Hello Worl1d")
    ]
}