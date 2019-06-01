# DIY：从零开始写一个 SQL 构建器

最近在项目中遇到了一个棘手的问题，因为 EF Core 不支持直接生成 `Update` 语句，所以这个项目就用到了 `EFCore.Plus` 来实现这个功能，但是 `EFCore.Plus` 对 SQLite 的支持并不是那么友好，在某些的情况下会生成出非常奇怪的 SQL 。
后来又在同一个项目中又遇到了 EF Core 在使用 SQL Server Provider 的 `UseRowNumberForPaging` 的选项后造成的各种更加奇怪的问题<span class="heimu">而且这些问题都快放了大半年了还没人修复</span>。

后来我就发现了一款船新的 SQL 构建器 —— [SqlKata](https://github.com/sqlkata/querybuilder)，它的语法非常的流畅，而且也很容易跟 EF 的表名映射[配合使用](https://github.com/ZeekoZhu/UtilsPack/tree/master/EFKata)。不过在使用过程中我又发现了它没办法生成基于计算结果的 `Update` 语句的问题，于是我想着顺手给他加个 BUFF 吧 —— 加上更加完善的 Update 生成方法。但是在阅读了 SqlKata 的源码之后，发现了它存在着不少问题。

众所周知，SQL 构建器替代的就是我们手动拼接 SQL 的操作，说到频繁的手动拼接字符串，大家肯定会想到用 `StringBuilder`，但是 SqlKata 没有。

另一方面，SqlKata 中有很多没有利用遍历结果而选择重新遍历的地方，我相信，如果使用 JetBrains 家的软件来检查这个项目中的代码的话，应该会有很多的 Warning 吧。

最终，我决定了自己来撸一个 SQL 构建器，看起来应该也不是什么困难的事 <span class="heimu">Flag</span>。

## DIY 的目标

首先需要明确一点，简单的 SQL 基本上都可以用 EF Core 生成，如果再去重复发明这种东西就真的是闲的蛋疼了，所以这个构建器需要足够灵活，至少要满足我们用它来写 SQL 小作文的基本要求。

然后这个构建器需要有足够的拓展性，一是为了能够方便地适配各种 SQL 方言，二是为了能在之后实现更加复杂的 SQL 生成能力（例如，`CASE ... WHEN`）。

说白了，其实要实现的真正功能就是 SQL 语法的 DSL。

## 理解 SQL

编写 DSL 的关键就在于 “Domain”，之后理解了 SQL 的语法，我们才能创造出正确的 DSL。在网上，我发现维基百科对于 SQL 语法的讲解还是比较详细的，具体可以看这里： https://zh.wikipedia.org/wiki/SQL语法 。

首先我们可以了解到， SQL 的主要语法元素有以下几种：

* Statement
* Clause
* Expression

我们向数据库发送的每一条指令都是一个完整的 Statement，每个 Statement 都是由若干个 Clause 构成的，每一个 Clause 中则根据特定的语法规则包含了若干个 Expression 。

所以 SQL Builder 的基本领域模型就出现了：

```fsharp
type SqlStatement =
  member val Clauses: SqlClause list with get, set
and SqlClause = 
  member val Expressions: SqlExpressoin list with get, set
and SqlExpressoin = class end
```

然而这个模型并不正确，理由如下：

1. 每一种 Statement 都有自己的可接受的 Clause 类型与数量
2. 有些操作符的操作数是 Statement 而不是 Expression，例如 `UNION`
3. 有些操作符的操作数甚至是 Clause，例如 T-SQL 中的 `ROW_NUMBER()`

所以之后我设计出来的第二个领域模型是这样的：

```fsharp
[<AbstractClass>]
type SqlExpression() = class end

[<AbstractClass>]
type SqlStatement() =
  inherit SqlExpressoin()

type SelectClause() =
  // member definitions...
```

在这个全新的领域中，万物基于 `SqlExpression`。接着为了贯彻领域规则，我们可以在构建特定的 Expression 或者 Clause 的时候限定所能接受的参数类型，这里以 `SELECT` clause 举例：

```fsharp
type ColumnsOperand =
    | Identifier of IdentifierExpression
    | Alias of AliasExpression<IdentifierExpression>
    | ComputeAlias of AliasExpression<OperatorExpression>

type SelectClause(exprs: ColumnsOperand []) =
    inherit SqlExpression()
    member val Columns = exprs with get, set
```

这里我使用了 F# 的 Union Type （相当于加强版的 C# 枚举类型，每一个枚举值可以定义成一种其他类型）来定义了 Select Clause 中允许传入的列类型： 标志符、标志符的别名、计算结果的别名。

这样我们就兼顾了 SQL 语法的灵活性以及严谨性。

其他的语法元素都是使用类似这种方式来进行创建的，在这里我就不赘述了，你可以在这里找到全部的语法元素定义： https://github.com/ZeekoZhu/Norm/blob/master/Norm/BuildingBlock.fs。

## 开始编译之前

将之前定义好的语法元素编译成合法的 SQL 语句最核心的操作，首先想一下我们的 DSL 编译器应该是个什么样子的。

我们把 Statement 作为参数传入，然后它给我们返回生成的 SQL 语句以及相关的 SQL 参数化查询的参数。但是别忘了，我们还需要为每种 SQL 方言进行一些特殊的处理，这些特殊的处理信息也应该在编译之前告诉编译器。所以最终我们的编译函数看起来应该是这个样子的：

```fsharp
type Compile = CompilerContext*SqlExpression -> string*Dictionary<string, object>
```

```csharp
delegate Compile = Func<CompilerContext, SqlExpression, (string, Dictionary<string, object>)>
```

在 `CompilerContext` 中，我们需要存储不同方言的定制化处理方式，它也是一个委托类型，参数与我们的编译函数相同，返回结果表示是否应该在处理完成后继续执行通用的编译流程：

```fsharp
type OnCompileExpression = CompilerContext * SqlExpression -> bool
```

一些标点符号以及常量的使用在不同的方言中也是不一样的，所以也需要一个简单的对象来承载：

```fsharp
type SpecialConstants =
    { IdentifierLeft: string
      IdentifierRight: string
      MemberAccessor: string
      True: string
      False: string
      StringQuote: string
    }
```

考虑到字符串拼接的性能问题，`StringBuilder` 肯定跑不掉，它在整个编译过程中都会用到，所以放在 `CompilerContext` 中也是个不错的选择。另一个会在整个编译过程中用到的就是参数列表了，在这里我也用同样的方式来处理。


最终的 `CompilerContext` 就像这样：

```fsharp
type CompilerContext =
    { Buffer: StringBuilder
      Constants: SpecialConstants
      OnCompileExpression: OnCompileExpression option
      mutable ExtraContext: obj option
      Paramneters: CSList<ParameterExpression>
    }
    member this.Params =
        let dict = CSDict<string, obj>()
        this.Paramneters
        |> Seq.iter
            ( fun p ->
                dict.Add(p.Name, p.Value)
            )
        dict
```

在针对 SQL 方言编译的时候考虑到有可能需要附加更多的信息，所以就添加了一个可变的对象，用来存储这类额外产生的信息。

不过这时候我们就可以发现，`compile` 函数其实就不需要返回值了，因为编译产生的结果最终都会存储在 `CompilerContext` 对象中，所以我们的编译函数的签名最终确定为：

```fsharp
type Compile = CompilerContext*SqlExpression -> unit
```

`unit` 相当于 Void，也就是什么都不返回，编译结果我们可以直接从 CompilerContext 中取得。

## 编译每一个语法元素

这里的编译思路我参考了 LINQ 的 `ExpressionTreeVisitor`，为每一个语法元素都量身定制了参数相同的编译方法。这里以 `SelectStament` 举例：

```fsharp
type compileSelectStatement (ctx: CompilerContext) (stmt: SelectStatement) =
    /// 检查是否有 Common Table Expression 查询
    if stmt.Ctes.Count > 0 then
        writeArray ", " compile (stmt.Ctes.ToArray()) ctx
    /// 对 Select 子句递归调用 compile 函数
    compile ctx stmt.Select
    write " " ctx
    /// 对 From 子句递归调用 compile 函数
    compile ctx stmt.From
    write " " ctx

    /// 对可选的 Where 子句递归调用 compile 函数
    compileOptionClause ctx stmt.Where
    write " " ctx
    /// 对可选的 GroupBy 子句递归调用 compile 函数
    compileOptionClause ctx stmt.Group
    write " " ctx
    /// 对可选的 GroupBy 子句递归调用 compile 函数
    compileOptionClause ctx stmt.Having
    write " " ctx
    /// 对可选的 OrderBy 子句递归调用 compile 函数
    compileOptionClause ctx stmt.Order
    write " " ctx
    /// 对可选的 Pagination 子句递归调用 compile 函数
    compileOptionClause ctx stmt.Pagiantion
```

每一个编译特定类型的编译函数就像这样使用了深度优先的方式确定其中的子元素的编译顺序，然后再把具体的编译子元素的流程转交给 `compile` 函数。

在 `compile` 函数中，可以先检查是否设置了方言的特殊编译处理方法，如果设置了，那么先交由这个处理方法来处理某个具体类型的 `SqlExpression` 的编译过程。
接着再把这个 `SqlExpression` 分发给特定的编译函数进行处理：

```fsharp
type compile (ctx: CompilerContext) (expr: SqlExpression) =
    let preventDefault =
        match ctx.OnCompileExpression with
        | None -> false
        | Some handler -> handler (ctx, expr)
    if preventDefault then ()
    else 
        match expr with
        | :? SqlStatement as stmt -> compileStatement ctx stmt
        // 省略了一堆分发处理的分支
        | :? PaginationClause as x -> compilePagination ctx x
        | _ -> failwithf "%s: Not supported yet!" (expr.GetType().Name)
```

更多具体语法元素的编译方法就不再赘述了，详细的处理可以在这里找到： https://github.com/ZeekoZhu/Norm/blob/master/Norm/Compiler/DefaultCompiler.fs

