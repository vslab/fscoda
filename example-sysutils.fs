module Example.Sys

type Camera = obj
type Decoder = obj

let getLowQualityCanvas () = "Low quality canvas"
let getHDCanvas () = "HD canvas"
let getImg (url:string) = printfn "Downloading image from %s" url
let getChannel () = "communication channel"
let receiveData s =
  printfn "Receiving data on %s" s
  "http://url/from/channel/image"
let take_picture camera =
  printfn "Taking picture using %A" camera
  "qr image"
let decode_qr decoder qr_picture =
  printfn "Decoding %A on %A" qr_picture decoder
  "http://url/from/qr/decoder/image"
let refreshUI s = printfn "Refreshing %s" s
