module Norm.Builder
open Norm.BuildingBlock

let table name = TableOrColumnExpression name
let column name = TableOrColumnExpression name

let dot table column = BinaryOperatorExpression(".", table, column, false)

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

let where (expr: SqlExpression) (query: SelectStatement) =
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

let orderBy columns (query: SelectStatement) =
    let clause =
        columns
        |> Seq.map (InternalBuilder.selectColumn)
        |> Array.ofSeq
        |> OrderClause
    query.Order <- Some clause
    query

let dbFn name values =
    let parameters =
        values
        |> Array.map InternalBuilder.valueExpr
    InvokeExpression(name, parameters)

let withCte cteSubQuery cteName (query: SelectStatement) =
    let withClause =
        WithClause((table cteName), cteSubQuery)
    query.Ctes.Add withClause
    query
    
let paging index perPage (query: SelectStatement) =
    query.Pagiantion <- Some (PaginationClause(index, perPage))
    query