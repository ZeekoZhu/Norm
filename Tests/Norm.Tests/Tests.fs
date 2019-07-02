module Tests

open Norm.Compiler;
open Norm.Tests
open FSharp.Control.Tasks.V2
open Microsoft.EntityFrameworkCore;
open Dapper
open Norm
open Norm.Builder

open Xunit

[<Fact>]
let ``Can Insert`` () =
    use db = DbContexts.useProvider DbContexts.pgsql
    db.Database.EnsureCreated() |> ignore
    let conn = db.Database.GetDbConnection()
    let compiler = DefaultCompiler.createDefaultContext()
    let blogs = table "Blogs"
    let blogPosts = table "BlogPosts"
    let tags = table "Tags"
    let categories = table "Categories"
    ()
//    task {
//        
//    }
