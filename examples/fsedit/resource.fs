module Resource

open System.Drawing
open System.IO


let private resource_dir = "resources"


let private exit_image_file = "application-exit.png"
let private new_image_file = "document-new.png"
let private open_image_file = "document-open.png"
let private save_image_file = "document-save.png"
let private save_as_image_file = "document-save-as.png"
let private copy_image_file = "edit-copy.png"
let private cut_image_file = "edit-cut.png"
let private paste_image_file = "edit-paste.png"
let private redo_image_file = "edit-redo.png"
let private undo_image_file = "edit-undo.png"
let private center_image_file = "format-justify-center.png"
let private left_image_file = "format-justify-left.png"
let private right_image_file = "format-justify-right.png"
let private bold_image_file = "format-text-bold.png"
let private italic_image_file = "format-text-italic.png"
let private underline_image_file = "format-text-underline.png"
let private strike_image_file = "text_strike.png"
let private rtfeditor_image_file = "mode-rtf-editor.png"
let private peditor_image_file = "mode-programming.png"
let private teditor_image_file = "mode-text-editor.png"

// File Action 
let New = Image.FromFile(Path.Combine(resource_dir, new_image_file))
let Open = Image.FromFile(Path.Combine(resource_dir, open_image_file))
let Save = Image.FromFile(Path.Combine(resource_dir, save_image_file))
let SaveAs = Image.FromFile(Path.Combine(resource_dir, save_as_image_file))
let Exit = Image.FromFile(Path.Combine(resource_dir, exit_image_file))

// 

// EDIT ACTIONS
let Copy = Image.FromFile(Path.Combine(resource_dir, copy_image_file))
let Cut = Image.FromFile(Path.Combine(resource_dir, cut_image_file))
let Paste = Image.FromFile(Path.Combine(resource_dir, paste_image_file))
let Undo = Image.FromFile(Path.Combine(resource_dir, undo_image_file))
let Redo = Image.FromFile(Path.Combine(resource_dir, redo_image_file))

// FORMAT ACTIONS
let Center = Image.FromFile(Path.Combine(resource_dir, center_image_file))
let Left = Image.FromFile(Path.Combine(resource_dir, left_image_file))
let Right = Image.FromFile(Path.Combine(resource_dir, right_image_file))

let Bold = Image.FromFile(Path.Combine(resource_dir, bold_image_file))
let Italic = Image.FromFile(Path.Combine(resource_dir, italic_image_file))
let Underline = Image.FromFile(Path.Combine(resource_dir, underline_image_file))
let Strike = Image.FromFile(Path.Combine(resource_dir, strike_image_file))


// MODE ACTIONS

let RTFEditor = Image.FromFile(Path.Combine(resource_dir, rtfeditor_image_file))
let PEditor = Image.FromFile(Path.Combine(resource_dir, peditor_image_file))
let TEditor = Image.FromFile(Path.Combine(resource_dir, teditor_image_file))

module Util = //
  open System.Windows.Forms               
  let (<~~) (e : IEvent<System.EventHandler,System.EventArgs>) clo = e.AddHandler (new System.EventHandler(clo))

  let (<!~) (e : IEvent<FormClosingEventHandler,FormClosingEventArgs>) clo = e.AddHandler (new FormClosingEventHandler(clo))



