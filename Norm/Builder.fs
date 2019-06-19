module Norm.Builder
open Norm.BuildingBlock

let table name = TableOrColumnExpression name
let column name = TableOrColumnExpression name

let dot column table = BinaryOperatorExpression(".", table, column, false)

let NullSelect = SelectClause([||])
module internal InternalBuilder =
    let condition (expr: SqlExpression) =
        match expr with
        | :? ConstantExpression as x -> WhereCondition.Boolean x
        | :? BinaryOperatorExpression as x -> WhereCondition.Predicate x
        | _ -> failwith "Invalid expression in condition expression"
    let dataSet (expr: SqlExpression) =
        match expr with
        | :? AliasExpression<IdentifierExpression> as x -> DataSet.TableAlias x
        | :? AliasExpression<SelectStatement> as x -> DataSet.SubQuery x
        | :? IdentifierExpression as x -> DataSet.Table x
        | :? JoinExpression as x -> DataSet.Join x
        | _ -> failwith "Invalid expression in 'FROM' clause"
    
    let from expr = expr |> dataSet |> FromClause
    let valueExpr (expr: SqlExpression) =
        match expr with
        | :? IdentifierExpression as x -> ValueParameter.Identifier x
        | :? ConstantExpression as x -> Const x
        | :? ParameterExpression as x -> Param x
        | :? OperatorExpression as x -> Computed x
        | _ -> failwith "Invalid expression for value expression"
        
        
    let selectColumn (expr: SqlExpression) =
        match expr with
        | :? AliasExpression<IdentifierExpression> as x -> ColumnsOperand.Alias x
        | :? AliasExpression<OperatorExpression> as x -> ColumnsOperand.ComputeAlias x
        | :? IdentifierExpression as x -> ColumnsOperand.Identifier x
        | _ -> failwith "Invalid expression in 'SELECT' clause"

let query from = SelectStatement((InternalBuilder.from from), NullSelect)
let join _type left right on =
    JoinExpression((InternalBuilder.dataSet left), (InternalBuilder.dataSet right), _type, on)

let leftJoin left right on = join JoinType.Left left right on
let rightJoin left right on = join JoinType.Right left right on
let innerJoin left right on = join JoinType.Inner left right on
let fullOuterJoin left right on = join JoinType.FullOuter left right on
let crossJoin left right on = join JoinType.Cross left right on

let alias origin name =
    AliasExpression(origin, TableOrColumnExpression(name)) :> IdentifierExpression

let equals right left =
    EqualsExpression(left, right)

let trueValue = TrueExpression()
let falseValue = FalseExpression()

let AND right left =
    AndAlsoExpression(left, right)
let OR right left =
    OrElseExpression(left, right)

let IN (range: ParameterExpression) left =
    InRangeExpression(left, range)

let INQuery (query: SelectStatement) left =
    InRangeExpression(left, query)

let NotIN (range: ParameterExpression) left =
    NotInRangeExpression(left, range)

let NotINQuery (query: SelectStatement) left =
    NotInRangeExpression(left, query)

let where<'t when 't :> SqlStatement> (expr: SqlExpression) (query: 't) =
    let condition =
        InternalBuilder.condition expr
    query.Where <- Some (WhereClause condition)
    query

let columnOf parent child =
    MemberAccessExpression(parent, column(child)) :> IdentifierExpression

let select columns (query: SelectStatement) =
    let selectClause =
        columns
        |> Seq.map (InternalBuilder.selectColumn)
        |> Array.ofSeq
        |> SelectClause
    query.Select <- selectClause
    query

let groupBy columns (query: SelectStatement) =
    let groupClause =
        columns
        |> Seq.map (InternalBuilder.selectColumn)
        |> Array.ofSeq
        |> GroupClause
    query.Group <- Some groupClause
    query

let having expr (query: SelectStatement) =
    let condition =
        InternalBuilder.condition expr
    query.Having <- Some (HavingClause condition)
    query

let orderBy columns desc (query: SelectStatement) =
    let columns =
        columns
        |> Seq.map (InternalBuilder.selectColumn)
        |> Array.ofSeq
    query.Order <- Some (OrderClause(columns, desc))
    query

let dbFn name values =
    let parameters =
        values
        |> Array.map InternalBuilder.valueExpr
    InvokeExpression(name, parameters)

let withCte cteSubQuery cteName (query: SelectStatement) =
    let cte = CteExpression(table cteName, cteSubQuery)
    match query.Ctes with
    | Some x ->
        x.Tables.Add(cte)
    | None ->
        let clause = WithClause(CSList())
        clause.Tables.Add(cte)
        query.Ctes <- Some clause
    query
    
let paging index perPage (query: SelectStatement) =
    query.Limit <- Some (LimitClause(index, perPage))
    query

let set col value =
    AssignmentExpression(col, value |> InternalBuilder.valueExpr)
let update table mutations = UpdateStatement(table, mutations)

let delete table columns = DeleteStatement(table, columns)

type BuilderContext =
    { mutable ParamSeq: int
    }
    member this.Param value =
        this.ParamSeq <- this.ParamSeq + 1
        let paramName = sprintf "p%i" this.ParamSeq
        ParameterExpression(paramName, value)

let createBuilderCtx () =
    { ParamSeq = 0 }


