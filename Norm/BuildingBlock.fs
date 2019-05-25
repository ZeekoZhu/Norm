namespace Norm

type CSList<'t> = System.Collections.Generic.List<'t>

module BuildingBlock =
    [<AbstractClass>]
    type SqlExpression() = class end

    type ConstantExpression(value: string, isString: bool) =
        inherit SqlExpression()
        member val Value = value with get, set
        member val IsString = isString with get, set


    type TrueExpression() =
        inherit ConstantExpression("TRUE", false)
    type FalseExpression() =
        inherit ConstantExpression("FALSE", false)

    type ParameterExpression(name: string, value: obj) =
        inherit SqlExpression()
        member val Name = name with get, set
        member val Value = value with get, set

    [<AbstractClass>]
    type OperatorExpression() =
        inherit SqlExpression()

    type UnaryOperatorExpression(op: string, operand: SqlExpression) =
        inherit OperatorExpression()
        member val Op = op with get, set
        member val Operand = operand with get, set

    type BinaryOperatorExpression(op: string, left: SqlExpression, right: SqlExpression, wrapOperand: bool) =
        inherit OperatorExpression()
        member val Op = op with get, set
        member val Left = left with get, set
        member val Right = right with get, set
        member val WrapOperand = wrapOperand with get, set

    type EqualsExpression(left, right) =
        inherit BinaryOperatorExpression(" = ", left, right, true)

    type AndAlsoExpression(left, right) =
        inherit BinaryOperatorExpression(" AND ", left, right, true)

    type OrElseExpression(left, right) =
        inherit BinaryOperatorExpression(" OR ", left, right, true)

    type InRangeExpression(left, right) =
        inherit BinaryOperatorExpression(" IN ", left, right, true)

    type NotInRangeExpression(left, right) =
        inherit BinaryOperatorExpression(" NOT IN ", left, right, true)

    [<AbstractClass>]
    type IdentifierExpression(name: string) =
        inherit SqlExpression()
        member val Name = name with get, set

    /// Table name or Column name
    type TableOrColumnExpression(name: string) =
        inherit IdentifierExpression(name)

    type MemberAccessExpression(parent: IdentifierExpression, child: IdentifierExpression) =
        inherit IdentifierExpression(parent.Name + "." + child.Name)
        member val Parent: IdentifierExpression = parent with get, set
        member val Child: IdentifierExpression = child with get, set

    type AliasExpression(origin: SqlExpression, alias: IdentifierExpression) =
        inherit IdentifierExpression(alias.Name)
        member val Origin = origin with get, set
        member val Alias = alias with get, set
    type AliasExpression<'t when 't :> SqlExpression>(origin: 't, alias: IdentifierExpression) =
        inherit AliasExpression(origin, alias)

    type ColumnsOperand =
        | Identifier of IdentifierExpression
        | Alias of AliasExpression<IdentifierExpression>
        | ComputeAlias of AliasExpression<OperatorExpression>
    type ValueParameter =
        | Identifier of IdentifierExpression
        | Const of ConstantExpression
        | Param of ParameterExpression
        | Computed of OperatorExpression
        member this.Unwarp =
            match this with
            | Identifier x -> x :> SqlExpression
            | Const x -> x :> SqlExpression
            | Param x -> x :> SqlExpression
            | Computed x -> x :> SqlExpression
    and InvokeExpression(name: string, parameters: ValueParameter []) =
        inherit OperatorExpression()
        member val Name = name with get, set
        member val Params = parameters with get, set

    and BetweenExpression(operand: ValueParameter, fromValue: ValueParameter, toValue: ValueParameter) =
        inherit OperatorExpression()
        member val Operand = operand with get, set
        member val From = fromValue with get, set
        member val To = toValue with get, set

    type SelectClause(exprs: ColumnsOperand []) =
        inherit SqlExpression()
        member val Columns = exprs with get, set

    type GroupClause(exprs: ColumnsOperand []) =
        inherit SqlExpression()
        member val Columns = exprs with get, set

    type OrderClause(exprs: ColumnsOperand []) =
        inherit SqlExpression()
        member val Columns = exprs with get, set


    [<AbstractClass>]
    type SqlStatement() =
        inherit SqlExpression()
        member val Where: WhereClause option = None with get, set

    and SelectStatement(from: FromClause, select: SelectClause) =
        inherit SqlStatement()
        member val From = from with get, set
        member val Select = select with get, set
        member val Group: GroupClause option = None with get, set
        member val Having: HavingClause option = None with get, set
        member val Order: OrderClause option = None with get, set
        member val Ctes: CSList<WithClause> = new CSList<WithClause>()
        member val Pagiantion: PaginationClause option = None with get, set

    and JoinType =
        | Inner
        | Left
        | Right
        | FullOuter
        | Cross

    and JoinExpression(left: DataSet, right: DataSet, _type: JoinType, on: OperatorExpression) =
        inherit SqlExpression()
        member val Left = left with get, set
        member val Right = right with get, set
        member val Type = _type with get, set
        member val On = on with get, set

    and DataSet =
        | SubQuery of AliasExpression<SelectStatement>
        | Join of JoinExpression
        | TableAlias of AliasExpression<IdentifierExpression>
        | Table of IdentifierExpression
        | SetOperation of SetOperationExpression
        member this.UnwrapDataSet =
            match this with
            | SubQuery x -> x :> SqlExpression
            | Join x -> x :> SqlExpression
            | TableAlias x -> x :> SqlExpression
            | Table x -> x :> SqlExpression
            | SetOperation x -> x :> SqlExpression

    and SetOperationExpression(op: string, left: DataSet, right: DataSet) =
        inherit BinaryOperatorExpression(op, left.UnwrapDataSet, right.UnwrapDataSet, true)

    and UnionExpression(left, right) =
        inherit SetOperationExpression(" UNION ", left, right)
    and UnionAllExpression(left, right) =
        inherit SetOperationExpression(" UNION ALL ", left, right)
    and IntersectExpression(left, right) =
        inherit SetOperationExpression(" INTERSECT ", left, right)
    and ExceptExpression(left, right) =
        inherit SetOperationExpression(" EXCEPT ", left, right)

    and FromClause(dataSet: DataSet) =
        inherit SqlExpression()
        member val DataSet = dataSet with get, set
    and WhereCondition =
        | Boolean of ConstantExpression
        | Predicate of OperatorExpression
    and WhereClause(predicate: WhereCondition) =
        inherit SqlExpression()
        member val Condition = predicate with get, set
    and HavingClause(predicate: WhereCondition) =
        inherit SqlExpression()
        member val Condition = predicate with get, set

    and WithClause(cteName: IdentifierExpression, selectStmt: SelectStatement) =
        inherit SqlExpression()
        member val CteName = cteName with get, set
        member val Select = selectStmt with get, set

    and PaginationClause(index: int, perPage: int) =
        inherit SqlExpression()
        member val Index = index with get, set
        member val PerPage = perPage with get, set

    type AssignmentExpression(left: TableOrColumnExpression, right: ValueParameter) =
        inherit SqlExpression()
        member val Left = left with get, set
        member val Right = right with get, set
    type UpdateStatement(table: TableOrColumnExpression, mutations: AssignmentExpression []) =
        inherit SqlStatement()
        member val Table = table with get, set
        member val Mutations = mutations with get, set
        member val Where: WhereClause option = None with get, set

    type DeleteStatement(table: TableOrColumnExpression, columns: TableOrColumnExpression []) =
        inherit SqlStatement()
        member val Table = table with get, set
        member val Columns = columns with get, set
        member val Where: WhereClause option = None with get, set

