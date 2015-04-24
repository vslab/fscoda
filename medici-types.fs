module Medici.Types

open CoDa

[<TypedPred>]
[<Code>]
let paziente_visibile (m:string, p:string) =
  if true then
    failwith "Solved by JIT"
  else
    Medici.MediciContext.paziente_visibile(m, p)

[<TypedPred>]
[<Code>]
let paziente_attivo (m:string, p:string) =
  if true then
    failwith "Solved by JIT"
  else
    Medici.MediciContext.paziente_attivo(m, p)

[<TypedPred>]
[<Code>]
let esame_da_fare (p:string, e:string) =
  if true then
    failwith "Solved by JIT"
  else
    Medici.MediciContext.esame_da_fare(p, e)

[<TypedPred>]
[<Code>]
let esame_attivo (p:string, e:string) =
  if true then
    failwith "Solved by JIT"
  else
    Medici.MediciContext.esame_attivo(p, e)

[<TypedPred>]
[<Code>]
let esame_fatto (p:string, e:string) =
  if true then
    failwith "Solved by JIT"
  else
    Medici.MediciContext.esame_fatto(p, e)

[<TypedPred>]
[<Code>]
let esame_medico (m:string, e:string) =
  if true then
    failwith "Solved by JIT"
  else
    Medici.MediciContext.esame_medico(m, e)
