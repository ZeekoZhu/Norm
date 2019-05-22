module Norm.Compiler.DefaultCompiler
open System.Text
open Norm.BuildingBlock

type SpecialConstants() =
    member val IdentifierLeft = "["
    member val IdentifierRight = "]"
    member val MemberAccessor = "."
    member val KeyWordAs = "AS"

let consts = SpecialConstants()
type CompilerContext(buffer: StringBuilder) =
    member val Constants = consts
    member val Buffer = buffer

module Buffer =
    type PairedSide =
        | Left
        | Right
    
    let writePaired (left: string) (right: string) (side: PairedSide) (ctx: CompilerContext) =
        match side with
        | Left -> left
        | Right -> right
        |> ctx.Buffer.Append
        |> ignore
    
    let ``write()`` = writePaired "(" ")"
    
    
    let write (str: string) (ctx: CompilerContext) = ctx.Buffer.Append str |> ignore
    let writeSpace ctx = write " " ctx
    let ``write[]`` ctx = writePaired consts.IdentifierLeft consts.IdentifierRight ctx
    let writeArray (div: string) (writeEle: _ -> 'a -> unit) (array: 'a []) ctx =
        match array with
        | [||] -> ()
        | [|a|] -> writeEle ctx a
        | _ ->
            for i in 0 .. array.Length - 2 do
                let ele = array.[i]
                writeEle ctx ele
                write div ctx
            writeEle ctx (array |> Array.last)

open Buffer

let rec compileAlias (ctx: CompilerContext) (alias: AliasExpression) =
    ``write()`` Left ctx
    compile ctx alias.Origin
    ``write()`` Right ctx
    writeSpace ctx
    write ctx.Constants.KeyWordAs ctx
    writeSpace ctx
    alias.Alias |> compile ctx

and compileMemberAccess ctx (expr: MemberAccessExpression) =
    compile ctx expr.Parent
    write "." ctx
    compile ctx expr.Child
    ()
and compileIdentifier (ctx: CompilerContext) (identifier: IdentifierExpression) =
    match identifier with
    | :? MemberAccessExpression as x -> compileMemberAccess ctx x
    | _ ->
        ``write[]`` Left ctx
        write identifier.Name ctx
        ``write[]`` Right ctx

and compileSelectOperand (ctx: CompilerContext) (operand: SelectOperand) =
    match operand with
    | Identifier x -> compile ctx x
    | IdAlias x -> compile ctx x
    | ComputeAlias x -> compile ctx x

and compileSelectClause (ctx: CompilerContext) (select: SelectClause) =
    write "SELECT " ctx
    writeArray ", " compileSelectOperand select.Columns ctx

and compileDataSet ctx dataSet =
    match dataSet with
    | SubQuery x -> compile ctx x
    | Join x -> compile ctx x
    | TableAlias x -> compile ctx x
    | Table x -> compile ctx x

and compileFromClause (ctx: CompilerContext) (from: FromClause) =
    write "FROM " ctx
    compileDataSet ctx from.DataSet
    write " " ctx


and compileJoin (ctx: CompilerContext) (join: JoinExpression) =
    compileDataSet ctx join.Left
    write " " ctx
    let joinType = 
        match join.Type with
        | JoinType.Left -> "LEFT JOIN"
        | JoinType.Right -> "RIGHT JOIN"
        | JoinType.Inner -> "INNER JOIN"
        | JoinType.FullOuter -> "FULL OUTER JOIN"
        | JoinType.Cross -> "CROSS JOIN"
    write joinType ctx
    write " " ctx
    compileDataSet ctx join.Right
    write " ON " ctx
    compile ctx join.On
    
and compileBinary (ctx: CompilerContext) (bin: BinaryOperatorExpression) =
    ``write()`` Left ctx
    compile ctx bin.Left
    write " " ctx
    write bin.Op ctx
    write " " ctx
    compile ctx bin.Right
    ``write()`` Right ctx
    

and compileSelectStatement (ctx: CompilerContext) (stmt: SelectStatement) =
    compile ctx stmt.Select
    write " " ctx
    compile ctx stmt.From
    ()

and compileStatement ctx (stmt: SqlStatement) =
    match stmt with
    | :? SelectStatement as selectStmt -> compileSelectStatement ctx selectStmt
    | _ -> failwith "Not supported yet!"
    

and compile (ctx: CompilerContext) (expr: SqlExpression) =
    match expr with
    | :? SqlStatement as stmt -> compileStatement ctx stmt
    | :? AliasExpression as x -> compileAlias ctx x
    | :? IdentifierExpression as x -> compileIdentifier ctx x
    | :? SelectClause as x -> compileSelectClause ctx x
    | :? FromClause as x -> compileFromClause ctx x
    | :? JoinExpression as x -> compileJoin ctx x
    | :? BinaryOperatorExpression as x -> compileBinary ctx x
    | _ -> failwith "Not supported yet!"
