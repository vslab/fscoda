[<CoDa.Code>]
module Editor.InitContext

open CoDa.Runtime
open Editor.Facts
open System.Windows.Forms

let tks = "\\b(auto|double|int|struct|break|else|long|switch|case|enum|register|typedef|char|extern|return|union|const|float|short|unsigned|continue|for|signed|void|default|goto|sizeof|volatile|do|if|static|while)\\b"  


[<CoDa.ContextInit>]
let initFacts () =
  tell <| execution_mode_("rtf")

  tell <| file_dialog_filter_("Rich Text File (*.rtf)|*.rtf", "rtf")
  tell <| file_dialog_filter_("C Source Code (*.c)|*.c", "programming")
  tell <| file_dialog_filter_("Plain Text File (*.txt)|*.txt", "text")

  tell <| file_dialog_file_ext_("*.rtf", "rtf")
  tell <| file_dialog_file_ext_("*.c", "programming")
  tell <| file_dialog_file_ext_("*.txt", "text")

  tell <| stream_type_(RichTextBoxStreamType.RichText,"rtf")
  tell <| stream_type_(RichTextBoxStreamType.PlainText,"programming")
  tell <| stream_type_(RichTextBoxStreamType.PlainText,"text")

  tell <| tokens_(tks)
  
