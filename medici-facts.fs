module Medici.Facts

open CoDa

let inline private fact head (x:string, y: string) =
  Fact(head, [| x :> obj ; y :> obj |])
let reparto_medico = fact "reparto_medico"
let tipo_medico = fact "tipo_medico"

let reparto_paziente = fact "reparto_paziente"
let esame_fatto = fact "esame_fatto_"
let esame_prescritto = fact "esame_prescritto"

let tipo_esame = fact "tipo_esame"
let prerequisito_esame = fact "prerequisito_esame"
