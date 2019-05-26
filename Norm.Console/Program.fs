open System.Text
open Norm.Builder
open Norm.BuildingBlock
open Norm.Compiler
open Norm.Compiler.DefaultCompiler

[<EntryPoint>]
let main argv =
    let msSql2008 = SqlServerCompiler.createDefaultContext ({UseRowNumberForPaging = true})
    let msSql2012 = SqlServerCompiler.createDefaultContext ({UseRowNumberForPaging = false})
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
        |> orderBy [postCol "BlogId";] true
        |> paging 2 15
    let blogIdCte = table "BlogId"
    let fakeQuery =
        query (leftJoin comment blogIdCte ((commentCol "BlogId") |> equals (columnOf blogIdCte "Id")))
        |> withCte cteQuery "BlogId"
        |> select [commentCol "Id"]
//    compile msSql2008 fakeQuery
    compile msSql2012 fakeQuery
    
    
//    printfn "%s" (msSql2008.Buffer.ToString())
    printfn "%s" (msSql2012.Buffer.ToString())
    0 // return an integer exit code
