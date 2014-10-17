module YPCompiler

open YieldProlog
open System
open System.IO

[<EntryPoint>]
let main args =
  printfn "// Compiler output follows.\n"
  YP.tell(Console.Out)
  YP.see(Console.In)
  let TermList = new Variable()
  let PseudoCode = new Variable()
  for l1 in Parser.parseInput(TermList) do
    for l2 in Compiler.makeFunctionPseudoCode(TermList, PseudoCode) do
      Compiler.convertFunctionCSharp(PseudoCode)
  YP.seen()
  0
