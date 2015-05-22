[<CoDa.Code>]
module Physicians.DeviceContext

open CoDa.Runtime
open Physicians.Facts

[<CoDa.ContextInit>]
let initFacts () =
  tell <| physician_exam("Dr. Cox", "ECG")
  tell <| physician_exam("Dr. Cox", "Blood test")
  tell <| physician_exam("Dr. Kelso", "CT scan")
  tell <| physician_exam("Dr. Reid", "EEG")
  tell <| physician_exam("Dr. Dorian", "Radiography")
  tell <| physician_exam("Dr. Turk", "Blood test")

  tell <| physician_has_hw("Dr. Kelso", "3D acceleration")

  tell <| physician_location("Dr. Cox", "Room 2")
  tell <| physician_location("Dr. Kelso", "Room 2")
  tell <| physician_location("Dr. Reid", "Room 1")
  tell <| physician_location("Dr. Dorian", "Room 3")
  // Missing location for Dr. Turk
  // tell <| physician_location("Dr. Turk", "Room 3")

  tell <| patient_location("Alice", "Room 2")
  tell <| patient_location("Bob", "Room 2")
  tell <| patient_location("Charlie", "Room 3")
  tell <| patient_location("Eve", "Room 1")

  tell <| patient_has_been_prescribed("Alice", "CT scan")
  tell <| patient_has_been_prescribed("Bob", "ECG")

  tell <| patient_has_done("Alice", "EEG")

  tell <| exam_requirement("ECG", "Blood test")
  tell <| exam_requirement("CT scan", "EEG")

  tell <| exam_view_hw("CT scan", "3D acceleration")
