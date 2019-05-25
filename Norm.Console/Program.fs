open System.Text
open Norm.Builder
open Norm.BuildingBlock
open Norm.Compiler.DefaultCompiler

[<EntryPoint>]
let main argv =
    let ctx = CompilerContext(StringBuilder())
    let idRange = ParameterExpression("idRange", [|1; 2; 3|])
    let postIdCount = ParameterExpression("count", 5)
    let blogPost = table "blog_Content"
    let comment = table "blog_Comment"
    let postCol = columnOf blogPost
    let commentCol = columnOf comment
    let joined = leftJoin blogPost comment (equals (postCol "Id") (commentCol "ParentId"))
    let cteQuery =
        query joined
        |> select [ postCol("Id"); (alias (commentCol "Id") "CommentId") ]
        |> where ((postCol("IsActive") |> equals trueValue) |> AND (postCol("Id") |> IN idRange))
        |> groupBy [postCol "BlogId";]
        |> having ((dbFn "sum" [| (postCol "PostId") |]) |> equals postIdCount)
        |> orderBy [postCol "BlogId";]
        |> paging 2 15
    let withBlogIdQuery =
        withCte cteQuery "BlogId"
    let blogIdCte = table "BlogId"
    query (leftJoin comment blogIdCte ((commentCol "BlogId") |> equals (columnOf blogIdCte "Id")))
    |> withBlogIdQuery
    |> select [commentCol "Id"]
    |> compile ctx
    
    
    printfn "%s" (ctx.Buffer.ToString())
    0 // return an integer exit code
