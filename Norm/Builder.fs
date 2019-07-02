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
    
    let binaryOp literal right left =
        BinaryOperatorExpression(literal, left, right, true)

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

let equalTo right left = InternalBuilder.binaryOp " = " right left
let notEqualTo right left= InternalBuilder.binaryOp " <> " right left
let gt right left = InternalBuilder.binaryOp " > " right left
let lt right left = InternalBuilder.binaryOp " < " right left
let gte right left = InternalBuilder.binaryOp " >= " right left
let lte right left = InternalBuilder.binaryOp " <= " right left
let like right left = InternalBuilder.binaryOp " LIKE " right left
let isOneOf right left = InternalBuilder.binaryOp " IN " right left
let isNotOneOf right left = InternalBuilder.binaryOp " NOT IN " right left
let isNull right left = InternalBuilder.binaryOp " IS NULL " right left
let isNotNull right left = InternalBuilder.binaryOp " IS NOT NULL " right left
let andAlso right left = InternalBuilder.binaryOp " AND " right left
let orElse right left = InternalBuilder.binaryOp " OR " right left


let trueValue = TrueExpression()
let falseValue = FalseExpression()
let nullValue = NullExpression()

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
    
let limitResult limit offset (query: SelectStatement) =
    query.Limit <- Some (LimitClause(limit, offset))
    query

let set col value =
    AssignmentExpression(col, value |> InternalBuilder.valueExpr)
let update table mutations = UpdateStatement(table, mutations)

let delete table = DeleteStatement(table)
let insert table columns values = InsertStatement(table, columns, values)

/// Builder for ValueParameter
module SqlValue =
    let id str = ValueParameter.Identifier (column(str))
    let constant str isString = ValueParameter.Const (ConstantExpression(str, isString))
    let param name value = ValueParameter.Param (ParameterExpression(name, value))
    let computed expr = ValueParameter.Computed expr

type BuilderContext =
    { mutable ParamSeq: int
    }
    member this.Param value =
        this.ParamSeq <- this.ParamSeq + 1
        let paramName = sprintf "p%i" this.ParamSeq
        ParameterExpression(paramName, value)

let createBuilderCtx () =
    { ParamSeq = 0 }


