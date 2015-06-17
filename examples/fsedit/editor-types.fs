module Editor.Types

open CoDa
open System.Windows.Forms

[<TypedPred>]
[<Code>]
let execution_mode(t : string) =
  if true then
    failwith "Solved by JIT"
  else
    Editor.EditorContext.execution_mode(t)

[<TypedPred>]
[<Code>]
let tokens(t : string) =
  if true then
    failwith "Solved by JIT"
  else
    Editor.EditorContext.tokens(t)
  

[<TypedPred>]
[<Code>]
let file_dialog_filter(t : string) =
  if true then
    failwith "Solved by JIT"
  else
    Editor.EditorContext.file_dialog_filter(t)

[<TypedPred>]
[<Code>]
let file_dialog_file_ext(t : string) =
  if true then
    failwith "Solved by JIT"
  else
    Editor.EditorContext.file_dialog_file_ext(t)

[<TypedPred>]
[<Code>]
let stream_type(t : RichTextBoxStreamType) =
  if true then
    failwith "Solved by JIT"
  else
    Editor.EditorContext.stream_type(t)


    
