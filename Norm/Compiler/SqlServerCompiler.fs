module Norm.Compiler.SqlServerCompiler
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

let getNextRowNumberColumn (ctx: CompilerContext) =
    let ctx = ctx.ExtraContext.Value :?> SqlServerContext
    ctx.RowNumberCtx <- ctx.RowNumberCtx + 1
    sprintf "__RowNumber_%i" ctx.RowNumberCtx

let getCurrentRowNumberColumn (ctx: CompilerContext) =
    let ctx = ctx.ExtraContext.Value :?> SqlServerContext
    sprintf "__RowNumber_%i" ctx.RowNumberCtx

let getCurrentRowNumberFrom (ctx: CompilerContext) =
    let ctx = ctx.ExtraContext.Value :?> SqlServerContext
    sprintf "__RowNumber_%i_from" ctx.RowNumberCtx

let getCurrentRowNumberTo (ctx: CompilerContext) =
    let ctx = ctx.ExtraContext.Value :?> SqlServerContext
    sprintf "__RowNumber_%i_to" ctx.RowNumberCtx

let convertToSelectRowNumber (select: SelectClause) orderBy (pagination: PaginationClause) (ctx: CompilerContext) =
    let rowNumber = RowNumberExpression(orderBy, pagination.Index, pagination.PerPage)
    let rowNumber = alias rowNumber (getNextRowNumberColumn ctx)
    let cols = Array.append select.Columns [|rowNumber |> ColumnsOperand.Identifier |]
    let newSelect = SelectClause(cols)
    newSelect

let convertToWhereRowNumber (pagination: PaginationClause) (ctx: CompilerContext) =
    let rowNumberCol = getCurrentRowNumberColumn ctx |> TableOrColumnExpression
    let rowNumberCol = rowNumberCol :> IdentifierExpression |> Identifier
    let fromValue = ParameterExpression((getCurrentRowNumberFrom ctx),((pagination.Index - 1) * pagination.PerPage + 1)) |> Param
    let toValue = ParameterExpression((getCurrentRowNumberTo ctx), pagination.PerPage * pagination.Index) |> Param
    let betweenCondition =
        BetweenExpression(rowNumberCol, fromValue, toValue)
    WhereClause(betweenCondition :> OperatorExpression |> Predicate)
    |> Some

let onCompileSelectStatement
    (ctx: CompilerContext) (stmt: SelectStatement) (compilerOpt: SqlServerCompilerOptions) =
    match ctx.ExtraContext with
    | None -> ctx.ExtraContext <- Some ({ RowNumberCtx = 0 } :> obj)
    | Some _ -> ()
    if compilerOpt.UseRowNumberForPaging && stmt.Pagiantion.IsSome then
        match stmt.Order with
        | None -> failwith "Pagination must have order by clause"
        | Some orderBy ->
            let newSelect =
                convertToSelectRowNumber stmt.Select orderBy stmt.Pagiantion.Value ctx
            stmt.Select <- newSelect
            let selectAll =
                SelectClause([|ColumnsOperand.Identifier (AllColumnsExpression())|])
            let newStmt =
                SelectStatement(FromClause(DataSet.SubQuery (AliasExpression<_>(stmt, TableOrColumnExpression("__RowNumber_Wrapper")))), selectAll)
            let newWhere =
                convertToWhereRowNumber stmt.Pagiantion.Value ctx
            newStmt.Where <- newWhere
            stmt.Pagiantion <- None
            compile ctx newStmt
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
        | _ -> false

let createDefaultContext (opt) =
    { DefaultCompiler.createDefaultContext () with
          OnCompileExpression = onCompileExpression opt |> Some
          Constants = defaultConstants
    }
