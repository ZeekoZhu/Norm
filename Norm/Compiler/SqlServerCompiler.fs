module Norm.Compiler.SqlServerCompiler
open System.Collections.Generic
open System.Text
open Norm.BuildingBlock
open Norm.Compiler.DefaultCompiler
open Norm.Compiler.DefaultCompiler.Buffer
open Norm.Builder

let defaultConstants =
    { Norm.Compiler.DefaultCompiler.defaultConstants with
          True = "1 = 1"
          False = "1 = 0"
    }

type SqlServerCompilerOptions =
    { UseRowNumberForPaging: bool }
    
type SqlServerContext =
    { mutable RowNumberCtx: int
      mutable GeneratedCte: Stack<CteExpression>
    }

type RowNumberExpression(orderBy: OrderClause, index:int, perPage: int) =
    inherit SqlExpression()
    member val OrderBy = orderBy with get, set
    member val Index = index with get, set
    member val PerPage = perPage with get, set

let compileRowNumber (ctx: CompilerContext) (rownumber: RowNumberExpression) =
    write "ROW_NUMBER() OVER( " ctx
    compileOrderBy ctx rownumber.OrderBy
    write " )" ctx

let onCompilePagination ctx (pagination: PaginationClause) (opt: SqlServerCompilerOptions) =
    if opt.UseRowNumberForPaging then
        true
    else
        let offset = (pagination.Index - 1) * pagination.PerPage
        write "OFFSET " ctx
        write (offset.ToString()) ctx
        write " ROWS FETCH NEXT " ctx
        write (pagination.PerPage.ToString()) ctx
        write " ROWS ONLY" ctx
        true

let useRowNumber (ctx: CompilerContext) =
    let ctx = ctx.ExtraContext.Value :?> SqlServerContext
    ctx.RowNumberCtx <- ctx.RowNumberCtx + 1

let getCurrentRowNumberColumn (ctx: CompilerContext) =
    let ctx = ctx.ExtraContext.Value :?> SqlServerContext
    sprintf "__RowNumber_%i" ctx.RowNumberCtx

let getCurrentRowNumberFrom (ctx: CompilerContext) =
    let ctx = ctx.ExtraContext.Value :?> SqlServerContext
    sprintf "__RowNumber_%i_from" ctx.RowNumberCtx

let getCurrentRowNumberTo (ctx: CompilerContext) =
    let ctx = ctx.ExtraContext.Value :?> SqlServerContext
    sprintf "__RowNumber_%i_to" ctx.RowNumberCtx

let getCurrentRowNumberWrapper (ctx: CompilerContext) =
    let ctx = ctx.ExtraContext.Value :?> SqlServerContext
    sprintf "__RowNumber_Wrapper_%i" ctx.RowNumberCtx

/// Convert
///     SELECT Foo.Bar, Foo.Baa
/// to
///     SELECT Foo.Bar, Foo.Baa, __RowNumber_1
let convertToSelectRowNumber (select: SelectClause) orderBy (pagination: PaginationClause) (ctx: CompilerContext) =
    let rowNumber = RowNumberExpression(orderBy, pagination.Index, pagination.PerPage)
    let rowNumber = alias rowNumber (getCurrentRowNumberColumn ctx)
    let cols = Array.append select.Columns [|rowNumber |> ColumnsOperand.Identifier |]
    let newSelect = SelectClause(cols)
    newSelect

/// Convert
/// SELECT Foo.Bar, Foo.Far AS Ffar
/// to
/// SELECT Bar, Ffar
let simplifySelectClause (select: SelectClause) =
    let removeColumnAccessor (operand: ColumnsOperand) =
        match operand with
        | Alias x -> x.Alias
        | ComputeAlias x -> x.Alias
        | ColumnsOperand.Identifier x ->
            match x with
            | :? MemberAccessExpression as ma ->
                ma.Child
            | :? AliasExpression as x -> x.Alias
            | _ -> x
        |> ColumnsOperand.Identifier
    select.Columns
    |> Array.map removeColumnAccessor
    |> SelectClause

/// Convert pagination to
/// WHERE __RowNumber_1 between @__RowNumber_1_from and @__RowNumber_1_to
let convertToWhereRowNumber (pagination: PaginationClause) (ctx: CompilerContext) =
    let rowNumberCol = getCurrentRowNumberColumn ctx |> TableOrColumnExpression
    let rowNumberCol = rowNumberCol :> IdentifierExpression |> Identifier
    let fromValue = ParameterExpression((getCurrentRowNumberFrom ctx),((pagination.Index - 1) * pagination.PerPage + 1)) |> Param
    let toValue = ParameterExpression((getCurrentRowNumberTo ctx), pagination.PerPage * pagination.Index) |> Param
    let betweenCondition =
        BetweenExpression(rowNumberCol, fromValue, toValue)
    WhereClause(betweenCondition :> OperatorExpression |> Predicate)
    |> Some

/// Convert
/// SELECT Foo.Bar FROM Foo
/// to
/// WITH [__RowNumber_Wrapper_1] AS (
///     SELECT Foo.Bar, __RowNumber_1 FROM Foo
/// )
/// SELECT Bar FROM [__RowNumber_Wrapper_1]
/// WHERE [__RowNumber_1] BETWEEN [..] AND [..]
let convertToRowNumberCTESelectStmt (stmt: SelectStatement) (ctx: CompilerContext) =
    useRowNumber ctx
    let selectInCte = convertToSelectRowNumber stmt.Select stmt.Order.Value stmt.Pagiantion.Value ctx
    let resultSelect = simplifySelectClause stmt.Select
    stmt.Select <- selectInCte
    let wrapperTable = table (getCurrentRowNumberWrapper ctx)
    let wrapperWhere = convertToWhereRowNumber stmt.Pagiantion.Value ctx
    stmt.Pagiantion <- None
    stmt.Order <- None
    let ctes = CSList()
    if stmt.Ctes.IsSome then
        ctes.AddRange stmt.Ctes.Value.Tables
        stmt.Ctes <- None
    let cte = CteExpression(wrapperTable, stmt)
    let wrapperStmt = SelectStatement(FromClause(DataSet.Table wrapperTable), resultSelect)
    let ctx = ctx.ExtraContext.Value :?> SqlServerContext
    ctx.GeneratedCte.Push(cte)
    wrapperStmt.Where <- wrapperWhere
    wrapperStmt

let onCompileWithClause
    (ctx: CompilerContext) (withClause: WithClause) (compilerOpt: SqlServerCompilerOptions) =
    if compilerOpt.UseRowNumberForPaging then
        let tempCtx =
            { ctx with Buffer = StringBuilder() }
        let sqlCtx = ctx.ExtraContext.Value :?> SqlServerContext
        DefaultCompiler.compileWithClause tempCtx withClause
        while sqlCtx.GeneratedCte.Count > 0 do
            write ", " tempCtx
            DefaultCompiler.compileCte tempCtx (sqlCtx.GeneratedCte.Pop())
        ctx.Buffer.Insert(0, tempCtx.Buffer.ToString()).Append(" ") |> ignore
        true
    else false

let onCompileSelectStatement
    (ctx: CompilerContext) (stmt: SelectStatement) (compilerOpt: SqlServerCompilerOptions) =
    match ctx.ExtraContext with
    | None ->
        ctx.ExtraContext <-
            Some ({ RowNumberCtx = 0;
                    GeneratedCte = Stack() } :> obj)
    | Some _ -> ()
    if compilerOpt.UseRowNumberForPaging then
        let stmt =
            if stmt.Pagiantion.IsSome then
                match stmt.Order with
                | None -> failwith "Query with pagination must have ORDER BY clause"
                | Some _ ->
                    let newStmt = convertToRowNumberCTESelectStmt stmt ctx
                    newStmt
            else stmt
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
        match stmt.Ctes with
        | Some x when x.Tables.Count > 0 ->
            compile ctx x
        | _ -> ()
        true
    else false

let onCompileExpression: SqlServerCompilerOptions -> OnCompileExpression =
    fun opt (ctx, expr) ->
        match expr with
        | :? SelectStatement as x ->
            onCompileSelectStatement ctx x opt
        | :? PaginationClause as x ->
            onCompilePagination ctx x opt
        | :? RowNumberExpression as x ->
            compileRowNumber ctx x
            true
        | :? WithClause as x ->
            onCompileWithClause ctx x opt
        | _ -> false

let createDefaultContext (opt) =
    { DefaultCompiler.createDefaultContext () with
          OnCompileExpression = onCompileExpression opt |> Some
          Constants = defaultConstants
    }
