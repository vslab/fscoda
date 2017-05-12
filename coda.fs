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

  let compiledExprs = Dictionary()

  let mutable private trace = false

  type private Context() =
    member this.Tell(Fact(name, args)) =
      if trace then
        printfn "Assert %A %A" name args
      YP.assertFact(Atom.a(name), args)

    member this.Retract(Fact(name, args)) =
      if trace then
        printfn "Retract %A %A" name args
      for _ in YP.retract(Functor.make(name, args)) do
        ()

    member this.Enumerate(goal:seq<bool>[], vars:Dictionary<string,obj>, vals:Dictionary<string,obj>) =
      if trace then
        printfn "Enumerate %A %A" goal vars
      let rec iterSols = function
      | h::t -> seq { for _ in h do for () in iterSols t do yield () }
      | [] ->
        if vars = null then
          seq { yield () }
        elif trace then
          seq {
            printfn "Solution:"
            for entry in vars do
              vals.[entry.Key] <- (entry.Value :?> Variable).getValue()
              printfn " %A -> %A" (entry.Key) (entry.Value)
            yield ()
          }
        else
          seq {
            for entry in vars do
              vals.[entry.Key] <- (entry.Value :?> Variable).getValue()
            yield ()
          }
      goal |> Array.toList |> iterSols

    member this.Solve(goal:seq<bool>[], solution:Dictionary<string,obj>) =
      if trace then
        printfn "Solve %A %A" goal solution
      let vars = if solution = null then null else new Dictionary<_,_>(solution)
      this.Enumerate(goal, vars, solution)
      |> Seq.tryPick Some
      |> Option.isSome

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

  let private genSym (boundvars:Set<_>) prefix =
    let rec genSymAux suffix =
      let name = sprintf "%s%d" prefix suffix
      if boundvars.Contains(name) then
        genSymAux (suffix+1)
      else
        name
    genSymAux 0

  let private solutionType = typeof<Dictionary<string,obj>>
  let private unitType = <@ () @>.Type
  let private currentProperty = typeof<System.Collections.Generic.IEnumerator<unit>>.GetProperty("Current")

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

  let private solVarToExpr vars solVar : Expr<Dictionary<string,obj>> =
    if Set.isEmpty vars then
      <@ null @>
    else
      Expr.Var(solVar) |> Expr.Cast

  let private makeSolBinding vars solVar body =
    let solExpr = solVarToExpr vars solVar
    let rec aux body = function
      | [] -> Expr.Let(solVar, <@ new Dictionary<string,obj>() @>, body)
      | hd::tl -> aux (Expr.Sequential(<@@ (%solExpr).[hd] <- new YieldProlog.Variable() @@>, body)) tl
    if Set.isEmpty vars then
      body
    else
      aux body (Set.toList vars)

  let rec private compileCoda (boundVars:Set<_>) (dboundVars:Map<_,_>) (vaVars:Set<_>) vaExpr expr =
    let compile = compileCoda boundVars dboundVars vaVars vaExpr
    let boxQuote q = Expr.Coerce(compile q, typeof<obj>)

    let compileGoal goal =
      let solVar = Var(genSym boundVars "solution", solutionType)
      let solExpr:Expr<Dictionary<string,obj>> = Expr.Var(solVar) |> Expr.Cast
      let ctxArgs =
        let tryCtxArgName  = function SpecificCall<@ (?) @>(None, _, [PropertyGet(None, ctx, []); Value(name, t)]) -> Some (name :?> string) | _ -> None
        let callCtxArgs = function Call(None, mi, args) -> List.choose tryCtxArgName args | p -> []
        let goalCtxArgs = function NewTuple(l) -> List.collect callCtxArgs l | g -> callCtxArgs g
        goal
        |> goalCtxArgs
        |> Set.ofList
      let goalExpr =
        let compileArg = function
          | SpecificCall<@ (?) @>(None, _, [PropertyGet(None, ctx, []); Value(name, t)]) ->
            let name = name :?> string
            <@ (%solExpr).[name] @>.Raw
          | Coerce(v, t) as expr -> Expr.Coerce(compile expr, t)
          | expr -> compile expr
        let compilePred = function Call(None, mi, args) -> Expr.Call(getTypedPred mi, List.map compileArg args) | p -> compile p
        let compileSubgoals = function NewTuple(l) -> List.map compilePred l | g -> [ compilePred g ]
        Expr.NewArray(typeof<seq<bool>>, compileSubgoals goal)
      ctxArgs,solVar,goalExpr

    match expr with
      // tell
      | SpecificCall<@ tell @>(None, _, [fact]) -> <@@ context.Tell(%%(compile fact)) @@>


      // retract
      | SpecificCall<@ retract @>(None, _, [fact]) -> <@@ context.Retract(%%(compile fact)) @@>


      // Behavioural variation
      | IfThenElse(SpecificCall<@ (!-) @>(None, _, [goal]), thenBody, elseBody) ->
        let vaVars,solVar,goal = compileGoal goal
        let boundVars = boundVars.Add(solVar.Name)
        let vaExpr = solVarToExpr vaVars solVar
        Expr.IfThenElse(<@ context.Solve(%%goal, %vaExpr) @>,
                        compileCoda boundVars dboundVars vaVars vaExpr thenBody,
                        compile elseBody)
        |> makeSolBinding vaVars solVar

      | SpecificCall<@ (?) @>(None, _, [PropertyGet(None, ctx, []); Value(name, t)]) -> // consume variable
        let name = name :?> string
        if vaVars.Contains(name) then
          Expr.Coerce(<@@ (%(vaExpr)).[name] @@>, expr.Type)
        else
          failwith "Cannot use undefined Context-Dependent variables"

      | SpecificCall<@ (?) @>(_,_,_) -> // (?) misuse
        failwith "The ? syntax is only allowed on the ctx object and only for solved goals"

      | SpecificCall<@ (!-) @>(_,_,_) -> // (!-) misuse
        failwith "The !- syntax is only allowed in behavioural variations"


      // for
      | Let(s, SpecificCall<@ (!--) @> (None, _, [goal]), body) ->
        let boundVars = boundVars.Add(s.Name)
        let vaVars,solVar,goal = compileGoal goal
        let boundVars = boundVars.Add(solVar.Name)
        let varVar = Var(genSym boundVars "variables", solutionType)
        let boundVars = boundVars.Add(varVar.Name)
        let valExpr = solVarToExpr vaVars solVar
        let varExpr = solVarToExpr vaVars varVar
        let expr = Expr.Let(s,
                            <@ context.Enumerate(%%goal, %varExpr, %valExpr) @>,
                            compileCoda boundVars dboundVars vaVars valExpr body)
        if Set.isEmpty vaVars then
          expr
        else
          Expr.Let(varVar, <@ new Dictionary<string,obj>(%valExpr) @>, expr)
        |> makeSolBinding vaVars solVar
        

      // Dlet
      | Let(var, SpecificCall<@ (|-) @>(None, _, [v; goal]), body) ->
        let name = var.Name
        let boundVars = boundVars.Add(name)
        let fallbackValue =
          match dboundVars.TryFind(name) with
            | None -> Expr.Coerce(<@@ raise <| InconsistentContext() @@>, v.Type)
            | Some lazyFallback -> Expr.Application(lazyFallback, <@ () @>)
        let vaVars,solVar,goal = compileGoal goal
        let boundVars = boundVars.Add(solVar.Name)
        let vaExpr = solVarToExpr vaVars solVar
        let value = Expr.IfThenElse(<@ context.Solve(%%goal, %vaExpr) @>,
                        compileCoda boundVars dboundVars vaVars vaExpr v,
                        fallbackValue)
        let lazyValue = Expr.Lambda(Var("unused", unitType),
                                    makeSolBinding vaVars solVar value)
        let lazyVar = Var(name, lazyValue.Type)
        let dboundVars = dboundVars.Add(name, Expr.Var(lazyVar))
        Expr.Let(lazyVar, lazyValue,
                 compileCoda boundVars dboundVars vaVars vaExpr body)

      | Let(var, value, body) -> // normal let
         let dboundVars = dboundVars.Remove(var.Name)
         Expr.Let(var, compile value,
                  compileCoda boundVars dboundVars vaVars vaExpr body)

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
          printfn "%s" (quot.Decompile())

        if types.Length <> args.Length then
          failwith "Mismatch between args and signature"

        if not (compiledExprs.ContainsKey(mi)) then
          let translated = compileCoda (Set.empty) (Map.empty) (Set.empty) (Unchecked.defaultof<_>) quot
          compiledExprs.[mi] <- translated
          if trace then
            printfn "translated to"
            printfn "%s" (translated.Decompile())

        let translated = compiledExprs.[mi]
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
      initContext (Assembly.Load((attr :?> ContextAttribute).Name))
    // run the entry point
    callTramp entryPoint args |> ignore

  let run() =
    trace <- false
    startEntryPoint [| |]

  let debug([<ParamArray>] args) =
    trace <- true
    printfn "Running %A" (Assembly.GetEntryAssembly())
    startEntryPoint [| |]
