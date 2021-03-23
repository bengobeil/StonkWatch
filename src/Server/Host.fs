module Server.Host

open Fable.Remoting.Server
open Fable.Remoting.AspNetCore
open Falco
open Falco.Routing
open Falco.HostBuilder
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection

let portfolioApi =
  { getPortfolio = fun () -> async {
    return SeedData.portfolio
  } }

let webApp =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue portfolioApi

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
       .UseFalco(endpoints)
       .UseRemoting webApp
       
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