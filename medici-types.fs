module Physicians.Types

open CoDa

[<TypedPred>]
[<Code>]
let physician_exam (m:string, e:string) =
  if true then
    failwith "Solved by JIT"
  else
    Physicians.PhysiciansContext.physician_exam(m, e)

[<TypedPred>]
[<Code>]
let physician_location (m:string, e:string) =
  if true then
    failwith "Solved by JIT"
  else
    Physicians.PhysiciansContext.physician_location(m, e)

[<TypedPred>]
[<Code>]
let physician_can_view_patient (m:string, p:string) =
  if true then
    failwith "Solved by JIT"
  else
    Physicians.PhysiciansContext.physician_can_view_patient(m, p)

[<TypedPred>]
[<Code>]
let patient_active_exam (p:string, e:string) =
  if true then
    failwith "Solved by JIT"
  else
    Physicians.PhysiciansContext.patient_active_exam(p, e)

[<TypedPred>]
[<Code>]
let patient_has_done (p:string, e:string) =
  if true then
    failwith "Solved by JIT"
  else
    Physicians.PhysiciansContext.patient_has_done(p, e)

[<TypedPred>]
[<Code>]
let physician_can_view_exam (p:string, e:string) =
  if true then
    failwith "Solved by JIT"
  else
    Physicians.PhysiciansContext.physician_can_view_exam(p, e)

