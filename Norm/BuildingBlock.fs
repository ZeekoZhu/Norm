namespace Norm

module BuildingBlock =
    [<AbstractClass>]
    type SqlExpression() = class end
    
    type VariableExpression (name: string) =
        inherit SqlExpression()
        member val Name = name with get, set
    
    [<AbstractClass>]
    type OperatorExpression() =
        inherit SqlExpression()
    
    type BinaryOperatorExpression(op: string, left: SqlExpression, right: SqlExpression, wrapOperand: bool) =
        inherit OperatorExpression()
        member val Op = op with get, set
        member val Left = left with get, set
        member val Right = right with get, set
        member val WrapOperand = wrapOperand with get, set
        
    /// Table name or Column name
    type IdentifierExpression(name: string) =
        inherit SqlExpression()
        member val Name = name with get, set
    
    type MemberAccessExpression(parent: IdentifierExpression, child: IdentifierExpression) =
        inherit IdentifierExpression(parent.Name + "." + child.Name)
        member val Parent: IdentifierExpression = parent with get, set
        member val Child: IdentifierExpression = child with get, set
    
    type AliasExpression(origin: SqlExpression, alias: IdentifierExpression) =
        inherit OperatorExpression()
        member val Origin = origin with get, set
        member val Alias = alias with get, set
    type AliasExpression<'t> when 't :> SqlExpression (origin: 't, alias: IdentifierExpression) =
        inherit AliasExpression(origin, alias)

    type SelectOperand =
        | Identifier of IdentifierExpression
        | IdAlias of AliasExpression<IdentifierExpression>
        | ComputeAlias of AliasExpression<OperatorExpression>
    type SelectClause(exprs: SelectOperand []) =
        inherit SqlExpression()
        member val Columns = exprs with get, set

    [<AbstractClass>]
    type SqlStatement() =
        inherit SqlExpression()
    
    type SelectStatement(from: FromClause, select: SelectClause) =
        inherit SqlStatement()
        member val From = from with get, set
        member val Select = select with get, set

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

    and FromClause(dataSet: DataSet) =
        inherit SqlExpression()
        member val DataSet = dataSet with get, set
