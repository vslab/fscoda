module Editor.Facts

open CoDa

open System.Windows.Forms

let execution_mode_(mode : string) = Fact("execution_mode_", [| mode :> obj |])
let stream_type_(stream : RichTextBoxStreamType, mode : string) = Fact("stream_type_", [| stream :> obj ; mode :> obj |])
let tokens_(ts : string) = Fact("tokens_", [| ts :> obj |])
let file_dialog_filter_(f : string, m : string) = Fact("file_dialog_filter_", [| f :> obj ; m :> obj |])
let file_dialog_file_ext_(e : string, m : string) = Fact("file_dialog_file_ext_", [| e :> obj ; m :> obj |])
