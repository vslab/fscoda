[<CoDa.Code>]
module Medici.DeviceContext

open CoDa.Runtime
open Medici.Facts
//open Medici.Types

[<CoDa.ContextInit>]
let initFacts () =
  tell <| tipo_medico("Dr. Frankenstein", "neurologia")
  tell <| tipo_medico("Dr. Jekyll", "psicologia")
  tell <| tipo_medico("Dr. Freud", "cardiologia")
  tell <| tipo_medico("Joseph Mengele", "pediatria")
  tell <| tipo_medico("Mr. Hyde", "infermieristica")

  tell <| reparto_medico("Dr. Frankenstein", "Reparto 1")
  tell <| reparto_medico("Dr. Jekyll", "Reparto 2")
  tell <| reparto_medico("Dr. Freud", "Reparto 1")
  tell <| reparto_medico("Joseph Mengele", "Reparto 1")
  tell <| reparto_medico("Mr. Hyde", "Reparto 3")

  tell <| reparto_paziente("Letterio Galletta", "Reparto 2")
  tell <| reparto_paziente("Pierpaolo Degano", "Reparto 3")
  tell <| reparto_paziente("Gianluigi Ferrari", "Reparto 3")
  tell <| reparto_paziente("Andrea Canciani", "Reparto 1")

  tell <| tipo_esame("ECG", "cardiologia")
  tell <| tipo_esame("EEG", "neurologia")
  tell <| tipo_esame("Lastra", "radiologia")
  tell <| tipo_esame("TAC", "neurologia")
  tell <| tipo_esame("Prelievo sanguigno", "infermieristica")

  tell <| prerequisito_esame("ECG", "Prelievo sanguigno")
  tell <| prerequisito_esame("TAC", "EEG")

  tell <| esame_prescritto("Andrea Canciani", "TAC")
  tell <| esame_prescritto("Letterio Galletta", "ECG")

  tell <| esame_fatto("Letterio Galletta", "Prelievo sanguigno")
