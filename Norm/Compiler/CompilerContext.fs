namespace Norm.Compiler
open System.Text
open Norm.BuildingBlock

type CSList<'t> = System.Collections.Generic.List<'t>
type CSDict<'k,'v> = System.Collections.Generic.Dictionary<'k,'v>
type SpecialConstants =
    { IdentifierLeft: string
      IdentifierRight: string
      MemberAccessor: string
      StringQuote: string
    }

type OnCompileExpression = CompilerContext * SqlExpression -> bool
and CompilerContext =
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

