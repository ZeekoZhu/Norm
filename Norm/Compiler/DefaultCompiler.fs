module Norm.Compiler.DefaultCompiler
open System.Text
open Norm.BuildingBlock
type CSList<'t> = System.Collections.Generic.List<'t>


let defaultConstants =
    { IdentifierLeft = "\""
      IdentifierRight = "\""
      MemberAccessor = "."
      StringQuote = "'"
    }

let consts = defaultConstants
let createDefaultContext () =
    {
        Buffer = StringBuilder()
        Constants = consts
        OnCompileExpression = None
        ExtraContext = None
        Paramneters = CSList<_>()
    }

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
        | [| a |] -> writeEle ctx a
        | _ ->
            for i in 0..array.Length - 2 do
                let ele = array.[i]
                writeEle ctx ele
                write div ctx
            writeEle ctx (array |> Array.last)

open Buffer

let rec compileAlias (ctx: CompilerContext) (alias: AliasExpression) =
    ``write()`` Left ctx
    compile ctx alias.Origin
    ``write()`` Right ctx
    write " AS " ctx
    alias.Alias |> compile ctx

and compileAllColumns ctx (_: AllColumnsExpression) =
    write "*" ctx
and compileMemberAccess ctx (expr: MemberAccessExpression) =
    compile ctx expr.Parent
    write "." ctx
    compile ctx expr.Child
    ()
and compileDbObject ctx (expr: TableOrColumnExpression) =
        ``write[]`` Left ctx
        write expr.Name ctx
        ``write[]`` Right ctx

and compileColumnsOperand (ctx: CompilerContext) (operand: ColumnsOperand) =
    match operand with
    | ColumnsOperand.Identifier x -> compile ctx x
    | ColumnsOperand.Alias x -> compile ctx x
    | ComputeAlias x -> compile ctx x

and compileSelectClause (ctx: CompilerContext) (select: SelectClause) =
    write "SELECT " ctx
    writeArray ", " compileColumnsOperand select.Columns ctx

and compileDataSet ctx (dataSet: DataSet) =
    compile ctx dataSet.UnwrapDataSet

and compileFromClause (ctx: CompilerContext) (from: FromClause) =
    write "FROM " ctx
    compileDataSet ctx from.DataSet
    write " " ctx

and compileWhereClause (ctx: CompilerContext) (where: WhereClause) =
    write "WHERE " ctx
    match where.Condition with
    | WhereCondition.Boolean x -> compile ctx x
    | WhereCondition.Predicate x -> compile ctx x

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

and compileOptionClause ctx clause =
    match clause with
    | Some clause -> compile ctx clause
    | _ -> ()

and compileSelectStatement (ctx: CompilerContext) (stmt: SelectStatement) =
    match stmt.Ctes with
    | Some x when x.Tables.Count > 0 ->
        compile ctx x
    | _ -> ()
    compile ctx stmt.Select
    write " " ctx
    compile ctx stmt.From
    write " " ctx

    compileOptionClause ctx stmt.Where
    write " " ctx
    compileOptionClause ctx stmt.Group
    write " " ctx
    compileOptionClause ctx stmt.Having
    write " " ctx
    compileOptionClause ctx stmt.Order
    write " " ctx
    compileOptionClause ctx stmt.Limit

and compileStatement ctx (stmt: SqlStatement) =
    match stmt with
    | :? SelectStatement as selectStmt -> compileSelectStatement ctx selectStmt
    | :? UpdateStatement as x -> compileUpdate ctx x
    | :? DeleteStatement as x -> compileDelete ctx x
    | :? InsertStatement as x -> compileInsert ctx x
    | _ -> failwith "Not supported yet!"

and compileConstant (ctx: CompilerContext) (constExpr: ConstantExpression) =
    if constExpr.IsString then
        write (ctx.Constants.StringQuote) ctx
        write (constExpr.Value) ctx
        write (ctx.Constants.StringQuote) ctx
    else
        write (constExpr.Value) ctx

and compileParameter ctx (param: ParameterExpression) =
    ctx.Paramneters.Add(param)
    write "@" ctx
    write (param.Name) ctx

and compileGroupBy ctx (groupBy: GroupClause) =
    write "GROUP BY " ctx
    writeArray ", " compileColumnsOperand groupBy.Columns ctx

and compileHaving ctx (having: HavingClause) =
    write "HAVING " ctx
    match having.Condition with
    | WhereCondition.Boolean x -> compile ctx x
    | WhereCondition.Predicate x -> compile ctx x

and compileOrderBy ctx (orderBy: OrderClause) =
    write "ORDER BY " ctx
    writeArray ", " compileColumnsOperand orderBy.Columns ctx
    if orderBy.Descending then
        write " DESC" ctx

and compileCte ctx (cte: CteExpression) =
    compile ctx cte.CteName
    write " AS (" ctx
    compile ctx cte.Select
    write ")" ctx
    
and compileWithClause ctx (cte: WithClause) =
    write "WITH " ctx
    writeArray ", " compileCte (cte.Tables.ToArray()) ctx

and compileInvokeParam ctx (param: ValueParameter) =
    compile ctx param.Unwarp

and compileInvoke ctx (invoke: InvokeExpression) =
    write invoke.Name ctx
    ``write()`` Left ctx
    writeArray ", " compileInvokeParam invoke.Params ctx
    ``write()`` Right ctx

and compileBetween ctx (between: BetweenExpression) =
    compile ctx between.Operand.Unwarp
    write " BETWEEN " ctx
    compile ctx between.From.Unwarp
    write " AND " ctx
    compile ctx between.To.Unwarp

and compileLimit ctx (limit: LimitClause) =
    write "LIMIT " ctx
    write (limit.Offset.ToString()) ctx
    write " OFFSET " ctx
    write ((limit.Offset).ToString()) ctx

and compileAssignment ctx (assignment: AssignmentExpression) =
    compile ctx assignment.Left
    write " = " ctx
    compile ctx assignment.Right.Unwarp
and compileUpdate ctx (update: UpdateStatement) =
    write "UPDATE " ctx
    compile ctx update.Table
    write " SET " ctx
    writeArray ", " compile update.Mutations ctx
    compileOptionClause ctx update.From
    write " " ctx
    compileOptionClause ctx update.Where

and compileDelete ctx (delete: DeleteStatement) =
    write "DELETE " ctx
    compile ctx delete.Table
    write " " ctx
    compileOptionClause ctx delete.From
    write " " ctx
    compileOptionClause ctx delete.Where

and compileInsertValues ctx (values: ValueParameter[]) =
    write "(" ctx
    writeArray ", " compileInvokeParam values ctx
    write ")" ctx

and compileInsert ctx (insert: InsertStatement) =
    write "INSERT INTO " ctx
    compile ctx insert.Table
    if insert.Columns.Length > 0 then
        write "( " ctx
        writeArray ", " compile insert.Columns ctx
        write " ) " ctx
    write "VALUES " ctx
    writeArray ", " compileInsertValues (Array.ofList insert.Values) ctx

and compileWhenClause ctx (expr: WhenClause) =
    write "WHEN " ctx
    compile ctx expr.Condition
    write " THEN " ctx
    compile ctx expr.Result

and compileSwitchLabel ctx (expr: SwitchLabelClause) =
    write "WHEN " ctx
    compile ctx expr.Value.Unwarp
    write " THEN " ctx
    compile ctx expr.Result

and compileSimpleCase ctx (expr: SwitchExpression) =
    write "CASE " ctx
    compile ctx expr.TestValue.Unwarp
    write " " ctx
    writeArray " " compileSwitchLabel expr.Cases ctx
    write " ELSE " ctx
    compile ctx expr.Default
    write " END"

and compileSearchedCase ctx (expr: CaseExpression) =
    write "CASE " ctx
    writeArray " " compileWhenClause expr.Cases ctx
    write " ELSE " ctx
    compile ctx expr.Default
    write " END"

and compile (ctx: CompilerContext) (expr: SqlExpression) =
    let preventDefault =
        match ctx.OnCompileExpression with
        | None -> false
        | Some handler -> handler (ctx, expr)
    if preventDefault then ()
    else 
        match expr with
        | :? SqlStatement as stmt -> compileStatement ctx stmt
        | :? AliasExpression as x -> compileAlias ctx x
        | :? SelectClause as x -> compileSelectClause ctx x
        | :? FromClause as x -> compileFromClause ctx x
        | :? WhereClause as x -> compileWhereClause ctx x
        | :? GroupClause as x -> compileGroupBy ctx x
        | :? HavingClause as x -> compileHaving ctx x
        | :? OrderClause as x -> compileOrderBy ctx x
        | :? WithClause as x -> compileWithClause ctx x
        | :? JoinExpression as x -> compileJoin ctx x
        | :? BinaryOperatorExpression as x -> compileBinary ctx x
        | :? MemberAccessExpression as x -> compileMemberAccess ctx x
        | :? AllColumnsExpression as x -> compileAllColumns ctx x
        | :? TableOrColumnExpression as x -> compileDbObject ctx x
        | :? ConstantExpression as x -> compileConstant ctx x
        | :? ParameterExpression as x -> compileParameter ctx x
        | :? InvokeExpression as x -> compileInvoke ctx x
        | :? BetweenExpression as x -> compileBetween ctx x
        | :? LimitClause as x -> compileLimit ctx x
        | _ -> failwithf "%s: Not supported yet!" (expr.GetType().Name)
