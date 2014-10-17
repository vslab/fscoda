module Example.Facts

open CoDa

let device (t:string) = Fact("device", [| t :> obj |])
let user_prefer (t:string) = Fact("user_prefer", [| t :> obj |])
let qr_decoder (dec:obj) = Fact("qr_decoder", [| dec :> obj |])
let battery_level (lvl:string) = Fact("battery_level", [| lvl :> obj |])
let screen_quality (q:string) = Fact("screen_quality", [| q :> obj |])
let supported_codec (q:string) = Fact("supported_codec", [| q :> obj |])

let orientation0 (t:string) = Fact("orientation0", [| t :> obj |])
let sscreen0 (t:string) = Fact("sscreen0", [| t :> obj |])
let supported_media0 (t:string) = Fact("supported_media0", [| t :> obj |])
let camera0 (cam:obj) = Fact("camera0", [| cam :> obj |])
