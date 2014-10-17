module Example.Sys

let getLowQualityCanvas () = "Low quality canvas"
let getHDCanvas () = "HD canvas"
let getImg (url:string) = printfn "Downloading image from %s" url
let getChannel () = "communication channel"
let receiveData s =
  printfn "Receiving data on %s" s
  "http://url/from/channel/image"
let take_picture camera =
  printfn "Taking picture using %s" camera
  "qr image"
let decode_qr decoder qr_picture =
  printfn "Decoding %s on %s" qr_picture decoder
  "http://url/from/qr/decoder/image"
let refreshUI s = printfn "Refreshing %s" s
