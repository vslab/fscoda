[<CoDa.Code>]
module Physicians.Test

open CoDa.Runtime
open Physicians.Types

let find_physician phy =
  try
    let loc = ctx?location |-
              physician_location(phy, ctx?location)
    loc
  with e ->
    printfn "WARNING: cannot locate %s:\n%A" phy e
    "unknown location"

let display_exam phy exam =
  match ctx with
  | _ when !- physician_can_view_exam(phy, exam) -> printfn " - %s" exam
  | _ -> printfn " - %s (current device cannot display the exam data)" exam

let display phy pat =
  match ctx with
  | _ when !- physician_can_view_patient(phy, pat) ->
    match ctx with
    | _ when !- patient_has_done(pat, ctx?e) ->
      printfn "%s sees that %s has done:" phy pat
      for _ in !-- patient_has_done(pat, ctx?exam) do
        display_exam phy ctx?exam
    | _ -> printfn "%s sees that %s has done no exam" phy pat

    let next_exam = "no exam" |- True
    let next_exam = ctx?exam |- (physician_exam(phy, ctx?exam),
                                 patient_active_exam(pat, ctx?exam))
    printfn "%s can perform %s on %s" phy next_exam pat
  | _ -> printfn "%s cannot view details on %s" phy pat

[<CoDa.Context("medici-ctx")>]
[<CoDa.Context("example-ctx")>]
[<CoDa.EntryPoint>]
let main () =
  display "Dr. Turk" "Bob"
  display "Dr. Cox" "Bob"
  printfn ""

  display "Dr. Cox" "Alice"
  display "Dr. Kelso" "Alice"
  printfn ""

  tell <| Physicians.Facts.patient_has_done("Alice", "TC")
  display "Dr. Cox" "Alice"
  display "Dr. Kelso" "Alice"
  printfn ""

  for _ in !-- (patient_active_exam("Bob", ctx?exam),
                physician_exam(ctx?physician, ctx?exam)) do
      printfn "%s (currently in %s) might perform %s on Bob"
        ctx?physician (find_physician ctx?physician) ctx?exam

do
  run ()
  //debug ()

