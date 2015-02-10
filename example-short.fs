[<CoDa.Code>]
module Example.Test

open CoDa.Runtime
open Example.Types
open Example.Sys

[<CoDa.Context("example-ctx")>]
[<CoDa.EntryPoint>]
let main () =
  match ctx with
    | _ when !- (direct_comm ()) ->
      let c = getChannel ()
      receiveData c

    | _ when !- (use_qrcode(ctx?decoder), camera(ctx?camera)) ->
      let p = take_picture (ctx?camera)
      decode_qr (ctx?decoder) p

  |> refreshUI

do debug ()
