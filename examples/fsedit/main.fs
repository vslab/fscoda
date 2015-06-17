[<CoDa.Code>]
module Editor.Main

open System
open System.Windows.Forms
open CoDa.Runtime

[<STAThread>]
[<CoDa.EntryPoint>]
[<CoDa.Context("editor-initctx")>]  
let main () =
  let mainWindow = Ui.mainForm ()
  Application.EnableVisualStyles ()
  Application.Run(mainWindow)

do debug ()  // run ()
