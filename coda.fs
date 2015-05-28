namespace CoDa

// ReflectedDefinitionAttribute cannot be subclassed because it is sealed
type Code = ReflectedDefinitionAttribute

type EntryPointAttribute() =
  inherit System.Attribute()

[<System.AttributeUsageAttribute(System.AttributeTargets.All, AllowMultiple = true)>]
type ContextAttribute(assemblyId:string) =
  inherit System.Attribute()
  member x.Name = assemblyId

type ContextInitAttribute() =
  inherit System.Attribute()

type TypedPredAttribute() =
  inherit System.Attribute()

type InconsistentContext() =
  inherit System.Exception("Context inconsistency detected")

type Fact = Fact of string * obj[]

module Runtime =
  open Microsoft.FSharp.Quotations
  open Microsoft.FSharp.Quotations.DerivedPatterns
  open Microsoft.FSharp.Quotations.ExprShape
  open Microsoft.FSharp.Quotations.Patterns
  open System
  open System.Collections.Generic
  open System.Reflection

  open Swensen.Unquote.Extensions
  open YieldProlog

  let mutable private trace = false

  let private genSym (boundvars:Set<_>) prefix =
    let rec genSymAux suffix =
      let name = sprintf "%s%d" prefix suffix
      if boundvars.Contains(name) then
        genSymAux (suffix+1)
      else
        name
    if boundvars.Contains(prefix) then
      genSymAux 0
    else
      prefix

  let rec private exprVars = function
    | ShapeCombination(comb,args) -> List.map exprVars args |> Set.unionMany
    | ShapeLambda(v,body) -> exprVars body |> Set.add v.Name
    | ShapeVar var -> Set.singleton var.Name

  type private Context() = 
    let enumType = typeof<IEnumerator<bool>>
    let castPred g : Expr<seq<bool>> = Expr.Cast g

    member this.Tell(Fact(name, args)) =
      if trace then
        printfn "Assert %A %A" name args
      YP.assertFact(Atom.a(name), args)

    member this.Retract(Fact(name, args)) =
      if trace then
        printfn "Retract %A %A" name args
      for _ in YP.retract(Functor.make(name, args)) do
        ()

    member this.QuoteEnumerate(goal:Expr[], body:Expr) = 
      if trace then
        printfn "QuoteEnumerate\n%A\n%s" [for g in goal -> g.Decompile()] (body.Decompile())
      
      body  
      |> Array.foldBack (fun g s -> <@@ for (x:bool) in %(castPred g) do %%s @@>) goal

    member this.QuoteSolve(goal:Expr[], thenExpr:Expr, elseExpr:Expr) =
      if trace then
        printfn "QuoteSolve\n%A\n%s\n%s" [for g in goal -> g.Decompile()] (thenExpr.Decompile()) (elseExpr.Decompile())
      let mutable vars = Set.union (exprVars thenExpr) (exprVars elseExpr)
      let enumVars = Array.zeroCreate goal.Length
      for i = 0 to goal.Length-1 do
        let name = genSym vars "enum"
        vars <- Set.add name vars
        enumVars.[i] <- Var(name, enumType)

      // use e0 = (%goal.[0]).GetEnumerator()
      // use e1 = (%goal.[1]).GetEnumerator()
      // ...
      // if e0.MoveNext && e1.MoveNext && .. then %thenExpr else %elseExpr

      let enumExprs:Expr<IEnumerator<bool>>[] = Array.map (Expr.Var >> Expr.Cast) enumVars
      let enums = goal |> Array.map (fun g -> <@ (%(castPred g)).GetEnumerator() @>)
      let disposes = enumExprs |> Array.map (fun e -> <@ (%e).Dispose() @>)
      let moves = enumExprs |> Array.map (fun e -> <@ (%e).MoveNext() @>)
      let guard = moves |> Array.reduceBack (fun a b -> <@ (%a) && (%b) @>)

      Expr.IfThenElse(guard, thenExpr, elseExpr)
      |> Array.foldBack (fun e s -> Expr.TryFinally(s, e)) disposes
      |> Array.foldBack2 (fun v e s -> Expr.Let(v, e, s)) enumVars enums

  // Global context, only accessible by the runtime
  let private context = new Context()

  // Convenience predicates
  let True = [ false ] :> seq<bool>
  let False = [ ] :> seq<bool>

  // CoDa operators
  let tell (x:Fact) : unit =
    failwith "Stub to be replaced by interpreter/compiler"

  let retract (x:Fact) : unit =
    failwith "Stub to be replaced by interpreter/compiler"

  let (|-) (x:'T) y : 'T =
    failwith "Stub to be replaced by interpreter/compiler"

  let (!-) y : bool =
    failwith "Stub to be replaced by interpreter/compiler"

  let (!--) y : seq<unit> =
    failwith "Stub to be replaced by interpreter/compiler"

  let (?) x y =
    failwith "Stub to be replaced by interpreter/compiler"

  // singleton used for id generation (together with (?))
  let ctx = obj()

  let private isCodaFunction = function MethodWithReflectedDefinition(quot) -> true | _ -> false

  let private varType = typeof<YieldProlog.Variable>
  let private unitType = <@ () @>.Type
  //let private currentProperty = typeof<System.Collections.Generic.IEnumerator<unit>>.GetProperty("Current")

  let private getTypedPred (mi:System.Reflection.MethodInfo) =
    if mi.GetCustomAttributes(typeof<TypedPredAttribute>, false).Length <> 0 then
      match mi with
      | MethodWithReflectedDefinition(quot) ->
        match quot with
        | Lambdas(_,IfThenElse(guard,thenExpr,elseExpr)) | IfThenElse(guard,thenExpr,elseExpr) ->
          match elseExpr with
          | (Lambdas(_,Call(_,mi,_)) | Call(_,mi,_)) -> mi
          | _ -> failwith "Expected Call"
        | _ -> failwith "Expected IfThenElse"
      | _ -> failwith "Expected ReflectedDefinition"
    else
      mi

  let private makeSolBinding vars body =
    Map.fold (fun s name v -> Expr.Let(v, <@@ new YieldProlog.Variable() @@>, s)) body vars

  let rec private compileCoda (boundVars:Set<_>) (dboundVars:Map<_,_>) (vaVars:Map<_,_>) expr =
    let compile = compileCoda boundVars dboundVars vaVars
    let boxQuote q = Expr.Coerce(compile q, typeof<obj>)

    let compileGoal goal =
      let tryCtxArgName  = function SpecificCall<@ (?) @>(None, _, [PropertyGet(None, ctx, []); Value(name, t)]) -> Some (name :?> string) | _ -> None
      let callCtxArgs = function Call(None, mi, args) -> List.choose tryCtxArgName args | p -> []
      let goalCtxArgs = function NewTuple(l) -> List.collect callCtxArgs l | g -> callCtxArgs g
      let distinctNames = goal |> goalCtxArgs |> Set.ofList |> Set.toArray
      let syms = Array.zeroCreate distinctNames.Length
      let mutable boundNames = boundVars
      for i = 0 to distinctNames.Length-1 do
        let name = genSym boundNames distinctNames.[i]
        boundNames <- boundNames.Add(name)
        syms.[i] <- Var(name, typeof<YieldProlog.Variable>)
      let ctxArgs = Array.zip distinctNames syms |> Map.ofArray
      let goalExpr =
        let compileArg = function
          | SpecificCall<@ (?) @>(None, _, [PropertyGet(None, ctx, []); Value(name, t)]) ->
            Expr.Var(ctxArgs.[name :?> string])
          | Coerce(v, t) as expr -> Expr.Coerce(compile expr, t)
          | expr -> compile expr
        let compilePred = function Call(None, mi, args) -> Expr.Call(getTypedPred mi, List.map compileArg args) | p -> compile p
        let compileSubgoals = function NewTuple(l) -> List.map compilePred l | g -> [ compilePred g ]
        compileSubgoals goal |> List.toArray
      ctxArgs,goalExpr

    match expr with
      // tell
      | SpecificCall<@ tell @>(None, _, [fact]) -> <@@ context.Tell(%%(compile fact)) @@>


      // retract
      | SpecificCall<@ retract @>(None, _, [fact]) -> <@@ context.Retract(%%(compile fact)) @@>


      // Behavioural variation
      | IfThenElse(SpecificCall<@ (!-) @>(None, _, [goal]), thenBody, elseBody) ->
        let vaVars,goals = compileGoal goal
        let boundVars = boundVars + Set(vaVars |> Map.toSeq |> Seq.map fst)
        let thenExpr = compileCoda boundVars dboundVars vaVars thenBody
        let elseExpr = compile elseBody
        context.QuoteSolve(goals, thenExpr, elseExpr)
        |> makeSolBinding vaVars

      | SpecificCall<@ (?) @>(None, _, [PropertyGet(None, ctx, []); Value(name, t)]) -> // consume variable
        match vaVars.TryFind(name :?> string) with
        | Some v ->
          let e:Expr<YieldProlog.Variable> = Expr.Var(v) |> Expr.Cast
          Expr.Coerce(<@@ (%e).getValue() @@>, expr.Type)
        | None -> failwith "Cannot use undefined Context-Dependent variables"

      | SpecificCall<@ (?) @>(_,_,_) -> // (?) misuse
        failwith "The ? syntax is only allowed on the ctx object and only for solved goals"

      | SpecificCall<@ (!-) @>(_,_,_) -> // (!-) misuse
        failwith "The !- syntax is only allowed in behavioural variations"


      // for
      | Let(s, SpecificCall<@ (!--) @> (None, _, [goal]),
            Let(_, _,
                TryFinally(WhileLoop(_,
                                     Let(_, _, body)), _))) ->
        let boundVars = boundVars.Add(s.Name)
        let vaVars,goals = compileGoal goal
        let boundVars = boundVars + Set(vaVars |> Map.toSeq |> Seq.map fst)
        let body = compileCoda boundVars dboundVars vaVars body
        context.QuoteEnumerate(goals, body)
        |> makeSolBinding vaVars

      | SpecificCall<@ (!--) @>(_,_,_) -> // (!-) misuse
        failwith "The !-- syntax is only allowed for context-dependent iteration"


      // Dlet
      | Let(var, SpecificCall<@ (|-) @>(None, _, [v; goal]), body) ->
        let name = var.Name

        let fallbackValue =
          match dboundVars.TryFind(name) with
            | None -> Expr.Coerce(<@@ raise <| InconsistentContext() @@>, v.Type)
            | Some lazyFallback -> Expr.Application(lazyFallback, <@ () @>)

        let boundVars = boundVars.Add(name)
        let vaVars,goals = compileGoal goal
        let boundVars = boundVars + Set(vaVars |> Map.toSeq |> Seq.map fst)
        let thenExpr = compileCoda boundVars dboundVars vaVars v
        let value = context.QuoteSolve(goals, thenExpr, fallbackValue)
        let lazyValue = Expr.Lambda(Var("unused", unitType),
                                    makeSolBinding vaVars value)
        let lazyVar = Var(name, lazyValue.Type)
        let dboundVars = dboundVars.Add(name, Expr.Var(lazyVar))
        Expr.Let(lazyVar, lazyValue,
                 compileCoda boundVars dboundVars vaVars body)

      | Let(var, value, body) -> // normal let
         let dboundVars = dboundVars.Remove(var.Name)
         Expr.Let(var, compile value,
                  compileCoda boundVars dboundVars vaVars body)

      | Var(v) ->
        match dboundVars.TryFind(v.Name) with
          | None -> expr
          | Some lazyFallback -> Expr.Application(lazyFallback, <@ () @>)


      | SpecificCall<@ (|-) @>(_,_,_) -> // (|-) misuse
        failwith "The |- syntax is only allowed in dynamic let bindings"


      // Trampoline
      | Call(target, mi, l) when isCodaFunction mi->
        if trace then
          printfn "Replacing call to %A" mi
        let args = [ for v in l -> boxQuote v ] // box arguments
        let args = match target with None -> args | Some e -> (boxQuote e) :: args // box optional "this"
        let args = Expr.NewArray(typeof<obj>, args) // prepare arguments array
        Expr.Coerce(<@@ callTramp mi %%args @@>, expr.Type)


      // General recursion
      | ShapeCombination(comb,args) -> ExprShape.RebuildShapeCombination(comb, List.map compile args)
      | ShapeLambda(v,body) -> Expr.Lambda(v, compile body)
      | ShapeVar var -> expr

  and private callTramp (mi:MethodInfo) (args:obj[]) =
    match mi with
      | MethodWithReflectedDefinition(quot) ->
        let types = mi.GetParameters()
        if trace then
          printfn "Trampoline! %A(%A) [%A/%A]" mi args (args.Length) (types.Length)
          printfn "%A" mi
          printfn "%s" <| quot.Decompile()

        if types.Length <> args.Length then
          failwith "Mismatch between args and signature"

        let translated = compileCoda (Set.empty) (Map.empty) (Map.empty) quot
        if trace then
          printfn "translated to"
          printfn "%s" <| translated.Decompile()

        let call =
          if args.Length = 0 then
            Expr.Application(translated, <@ () @>)
          else
            let args = args |> Array.mapi (fun i v -> Expr.Coerce(<@ v @>, types.[i].ParameterType))
            Expr.Applications(translated, [ for a in args -> [ a ]])

        call.Eval()

      | _ -> failwith "Trapoline on a non-reflected method"

  let private getMethods<'T> (assembly:Assembly) =
    let flags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static
    [ for typ in assembly.GetTypes() do
      for mi in typ.GetMethods(flags) do
      if mi.GetCustomAttributes(typeof<'T>, false).Length <> 0 then yield mi ]

  let private initContext assembly =
    for m in getMethods<ContextInitAttribute>(assembly) do
      callTramp m [| |] |> ignore

  let private startEntryPoint args =
    let entryPoint =
      match getMethods<EntryPointAttribute>(Assembly.GetEntryAssembly()) with
        | [it] -> it // exactly one entry point
        | _ -> failwith "Entry point not found!"

    for attr in entryPoint.GetCustomAttributes(typeof<ContextAttribute>, false) do
      initContext <| Assembly.Load((attr :?> ContextAttribute).Name)
    // run the entry point
    callTramp entryPoint args |> ignore

  let run() =
    trace <- false
    startEntryPoint [| |]

  let debug([<ParamArray>] args) =
    trace <- true
    printfn "Running %A" <| Assembly.GetEntryAssembly()
    startEntryPoint [| |]
