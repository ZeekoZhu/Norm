namespace Norm.Compiler

open Norm.BuildingBlock
open Norm.Compiler.DefaultCompiler
open Norm.Compiler.DefaultCompiler.Buffer


type BaseCompiler() =
    member __.CompileSelectStatement(ctx, stmt) =
        compileSelectStatement ctx stmt
    member __.CompileUpdateStatement(ctx, stmt) =
        compileUpdate ctx stmt
    member __.CompileDeleteStatement(ctx, stmt) =
        compileDelete ctx stmt
    member this.CompileStatement(ctx, stmt: SqlStatement) =
        match stmt with
        | :? SelectStatement as selectStmt -> this.CompileSelectStatement(ctx, selectStmt)
        | :? UpdateStatement as x -> this.CompileUpdateStatement(ctx, x)
        | :? DeleteStatement as x -> this.CompileDeleteStatement(ctx, x)
        | _ -> failwith "Not supported yet!"
        write ";" ctx

    member this.CompileAlias(ctx, x) =
        compileAlias ctx x

    member this.CompileSelectClause(ctx, x) =
        compileSelectClause ctx x

    member this.CompileFromClause(ctx, x) =
        compileFromClause ctx x

    member this.CompileWhereClause(ctx, x) =
        compileWhereClause ctx x

    member this.CompileGroupBy(ctx, x) =
        compileGroupBy ctx x
    member this.CompileHaving(ctx, x) =
        compileHaving ctx x
    member this.CompileOrderBy(ctx, x) =
        compileOrderBy ctx x
    member this.CompileWithCte(ctx, x) =
        compileWithCte ctx x
    member this.CompileJoin(ctx, x) =
        compileJoin ctx x
    member this.CompileBinary(ctx, x) =
        compileBinary ctx x
    member this.CompileMemberAccess(ctx, x) =
        compileMemberAccess ctx x
    member this.CompileDbObject(ctx, x) =
        compileDbObject ctx x
    member this.CompileConstant(ctx, x) =
        compileConstant ctx x
    member this.CompileParameter(ctx, x) =
        compileParameter ctx x
    member this.CompileInvoke(ctx, x) =
        compileInvoke ctx x
    member this.CompileBetween(ctx, x) =
        compileBetween ctx x
    member this.CompilePagination(ctx, x) =
        compilePagination ctx x
    member this.Compile (ctx: CompilerContext) (expr: SqlExpression) =
        match expr with
        | :? SqlStatement as stmt -> this.CompileStatement(ctx, stmt)
        | :? AliasExpression as x -> this.CompileAlias(ctx, x)
        | :? SelectClause as x -> this.CompileSelectClause(ctx, x)
        | :? FromClause as x -> this.CompileFromClause(ctx, x)
        | :? WhereClause as x -> this.CompileWhereClause(ctx, x)
        | :? GroupClause as x -> this.CompileGroupBy(ctx, x)
        | :? HavingClause as x -> this.CompileHaving(ctx, x)
        | :? OrderClause as x -> this.CompileOrderBy(ctx, x)
        | :? WithClause as x -> this.CompileWithCte(ctx, x)
        | :? JoinExpression as x -> this.CompileJoin(ctx, x)
        | :? BinaryOperatorExpression as x -> this.CompileBinary(ctx, x)
        | :? MemberAccessExpression as x -> this.CompileMemberAccess(ctx, x)
        | :? TableOrColumnExpression as x -> this.CompileDbObject(ctx, x)
        | :? ConstantExpression as x -> this.CompileConstant(ctx, x)
        | :? ParameterExpression as x -> this.CompileParameter(ctx, x)
        | :? InvokeExpression as x -> this.CompileInvoke(ctx, x)
        | :? BetweenExpression as x -> this.CompileBetween(ctx, x)
        | :? PaginationClause as x -> this.CompilePagination(ctx, x)
        | _ -> failwithf "%s: Not supported yet!" (expr.GetType().Name)
