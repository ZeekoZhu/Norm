open System.Text
open Norm.Builder
open Norm.Compiler.DefaultCompiler

[<EntryPoint>]
let main argv =
    let ctx = CompilerContext(StringBuilder())
    let blogPost = table "blog_Content"
    let comment = table "blog_Comment"
    let postCol = columnOf blogPost
    let commentCol = columnOf comment
    let joined = leftJoin blogPost comment (equals (postCol "Id") (commentCol "PostId"))
    query joined
    |> select [ postCol("Id"); commentCol("Id") ]
    |> compile ctx
    
    
    printfn "%s" (ctx.Buffer.ToString())
    0 // return an integer exit code
