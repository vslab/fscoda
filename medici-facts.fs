module Physicians.Facts

open CoDa

let inline private fact head (x:string, y: string) =
  Fact(head, [| x :> obj ; y :> obj |])

let physician_location = fact "physician_location_"
let physician_exam = fact "physician_exam_"
let physician_has_hw = fact "physician_has_hw_"

let patient_location = fact "patient_location_"
let patient_has_done = fact "patient_has_done_"
let patient_has_been_prescribed = fact "patient_has_been_prescribed_"

let exam_requirement = fact "exam_requirement_"
let exam_view_hw = fact "exam_view_hw_"
