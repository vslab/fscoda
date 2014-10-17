module Example.Types

open CoDa

[<TypedPred>]
[<Code>]
let video (t:string) =
  if true then
    failwith "Solved by JIT"
  else
    Example.ExampleContext.video(t)

[<TypedPred>]
[<Code>]
let use_qrcode (decoder:obj) =
  if true then
    failwith "Solved by JIT"
  else
    Example.ExampleContext.use_qrcode(decoder)

[<TypedPred>]
[<Code>]
let orientation (fmt:string) =
  if true then
    failwith "Solved by JIT"
  else
    Example.ExampleContext.orientation(fmt)

[<TypedPred>]
[<Code>]
let sscreen (size:string) =
  if true then
    failwith "Solved by JIT"
  else
    Example.ExampleContext.sscreen(size)

[<TypedPred>]
[<Code>]
let supported_media (fmt:string) =
  if true then
    failwith "Solved by JIT"
  else
    Example.ExampleContext.supported_media(fmt)

[<TypedPred>]
[<Code>]
let camera (cam:obj) =
  if true then
    failwith "Solved by JIT"
  else
    Example.ExampleContext.camera(cam)

[<TypedPred>]
[<Code>]
let direct_comm () =
  if true then
    failwith "Solved by JIT"
  else
    Example.ExampleContext.direct_comm()
