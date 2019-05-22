module Norm.Builder
open Norm.BuildingBlock

let table name = IdentifierExpression name
let column name = IdentifierExpression name
let dot table column = BinaryOperatorExpression(".", table, column, false)

let NullSelect = SelectClause([||])
module internal InternalBuilder =
    let dataSet (expr: SqlExpression) =
        match expr with
        | :? IdentifierExpression as x -> DataSet.Table x
        | :? AliasExpression<IdentifierExpression> as x -> DataSet.TableAlias x
        | :? AliasExpression<SelectStatement> as x -> DataSet.SubQuery x
        | :? JoinExpression as x -> DataSet.Join x
        | _ -> failwith "Invalid expression in 'FROM' clause"
    
    let from expr = expr |> dataSet |> FromClause
    
    let selectColumn (expr: SqlExpression) =
        match expr with
        | :? IdentifierExpression as x -> SelectOperand.Identifier x
        | :? AliasExpression<IdentifierExpression> as x -> SelectOperand.IdAlias x
        | :? AliasExpression<OperatorExpression> as x -> SelectOperand.ComputeAlias x
        | _ -> failwith "Invalid expression in 'SELECT' clause"

let query from = SelectStatement((InternalBuilder.from from), NullSelect)
let join _type left right on =
    JoinExpression((InternalBuilder.dataSet left), (InternalBuilder.dataSet right), _type, on)

let leftJoin left right on = join JoinType.Left left right on
let rightJoin left right on = join JoinType.Right left right on
let innerJoin left right on = join JoinType.Inner left right on
let fullOuterJoin left right on = join JoinType.FullOuter left right on
let crossJoin left right on = join JoinType.Cross left right on

let equals left right =
    BinaryOperatorExpression(" = ", left, right, true)

let columnOf parent child =
    MemberAccessExpression(parent, column(child))

let select columns (query: SelectStatement) =
    let selectClause =
        columns
        |> Seq.map (InternalBuilder.selectColumn)
        |> Array.ofSeq
        |> SelectClause
    query.Select <- selectClause
    query