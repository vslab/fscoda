[<CoDa.Code>]
module Medici.Test

open CoDa.Runtime
//open Medici.Facts
open Medici.Types

[<CoDa.Context("medici-ctx")>]
[<CoDa.EntryPoint>]
let main () =
  let updates = seq {
    yield ()
    tell <| Medici.Facts.esame_fatto("Andrea Canciani", "EEG")
    yield ()
    tell <| Medici.Facts.esame_fatto("Andrea Canciani", "TAC")
    yield ()
  }

  let storia_paziente = "" |- True
  for _ in updates do
    for _ in !-- paziente_visibile(ctx?medico, ctx?paziente) do
      let m = ctx?medico
      let p = ctx?paziente
      let storia_paziente =
        (let mutable s = " e sa che ha fatto:" in
         for _ in !-- esame_fatto(p, ctx?esame) do
           s <- sprintf "%s\n - %s" s (ctx?esame) 
         s) |- esame_fatto(p, ctx?es)

      match ctx with
        | _ when !- (paziente_visibile(m, p),
          esame_medico(m, ctx?esame),
          esame_attivo(p, ctx?esame)) -> printfn "%s deve fare l'esame \"%s\" su %s%s" m (ctx?esame) p storia_paziente

        //| _ when !- paziente_attivo(m,p) -> printfn "%s deve fare esami su %s%s" m p storia_paziente
        | _ -> printfn "%s vede %s%s" m p storia_paziente
    printfn ""

do
  run ()
  //debug ()
