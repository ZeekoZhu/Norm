module Norm.Tests.DbContexts
open Microsoft.EntityFrameworkCore
open Norm.Tests.DbContext

let useProvider fn =
    let dbBuilder = DbContextOptionsBuilder()
    new FooDbContext(fn dbBuilder)

let pgsql (dbBuilder: DbContextOptionsBuilder) =
    dbBuilder.UseNpgsql("Server=localhost;Port=25432;User ID=postgres;Password=p@55w0rd;Database=Norm")

let mysql (dbBuilder: DbContextOptionsBuilder) =
    dbBuilder.UseMySql("Server=localhost;Port=23306;Uid=root;Pwd=p@55w0rd;Database=Norm")
 
let mssql (dbBuilder: DbContextOptionsBuilder) =
    dbBuilder.UseSqlServer("Data Source=localhost,21433;Database=Norm;User ID=sa;Password=p@55w0rd")

let sqlite (dbBuilder: DbContextOptionsBuilder) =
    dbBuilder.UseSqlite("Data Source=./norm.db")

