[<CoDa.Code>]
module Example.DeviceContext

open CoDa.Runtime
open Example.Facts
open Example.Types
open Example.Sys

[<CoDa.ContextInit>]
let initFacts () =
  tell <| device("camera")
  tell <| camera0(new Camera())
  tell <| user_prefer("qr_code")
  tell <| qr_decoder(new Decoder())
