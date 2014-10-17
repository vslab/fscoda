[<CoDa.Code>]
module Example.Test

open CoDa.Runtime
open Example.Facts
open Example.Types
open Example.Sys

let dletExample () =
  let vcanvas = getLowQualityCanvas () |- video("low")
  let vcanvas = getHDCanvas () |- video("hd")
  refreshUI vcanvas

let getRemoteData url =
  match ctx with
    | _ when !- (orientation("landscape"), sscreen("large"), supported_media("png")) ->
      url + "-large.png"
    | _ when !- (orientation("portrait"), sscreen("small"), supported_media("svg")) ->
      url + "-small.svg"
  |> getImg

let getExhibitData () =
  match ctx with
    | _ when !- (direct_comm ()) ->
      let c = getChannel ()
      receiveData c

    | _ when !- (use_qrcode(ctx?decoder), camera(ctx?camera)) ->
      let p = take_picture (ctx?camera)
      decode_qr (ctx?decoder) p

  |> getRemoteData


let initFacts () =
  tell <| screen_quality("hd")
  tell <| battery_level("normal")
  tell <| device("camera")
  tell <| camera0("fake system camera")
  tell <| sscreen0("large")
  tell <| orientation0("landscape")
  tell <| user_prefer("qr_code")
  tell <| qr_decoder("fake system decoder")
  tell <| supported_media0("png")
  tell <| supported_codec("H.264")
  
[<CoDa.EntryPoint>]
let main () =
  initFacts ()

  dletExample ()
  let img = getExhibitData ()
  ()

do
  //run ()
  debug ()
