module Actions

open System.Drawing
open System.Windows.Forms

// Constants

let NewText = "New"
let OpenText = "Open"
let SaveText = "Save"
let SaveAsText = "Save as"
let ExitText = "Exit"

let CutText = "Cut"
let CopyText = "Copy"
let PasteText = "Paste"
let UndoText = "Undo"
let RedoText = "Redo"

let BoldText = "Bold"
let ItalicText = "Italic"
let UnderlineText = "Underline"
let LeftText = "Left"
let CenterText = "Center"
let RightText = "Right"

let RTFEditorText = "RTF Editor"
let PEditorText = "Programming Editor"
let TEditorText = "Notepad"

type t = { text : string; image : Image option; toggle : bool; enable : bool }

let private DefaultAction = { text = ""; image = None; toggle = false; enable = true }


let FileActions =
  [{ DefaultAction with text = NewText; image = Some( Resource.New ) } ;
   { DefaultAction with text = OpenText; image = Some( Resource.Open  )  } ;
   { DefaultAction with text = SaveText; image = Some( Resource.Save )  } ;
   { DefaultAction with text = SaveAsText; image = Some( Resource.SaveAs )  } ;
   { DefaultAction with text = ExitText; image = Some( Resource.Exit ) }
   ]

let EditActions =
  [ { DefaultAction with text = CutText; image = Some( Resource.Cut ) };
    { DefaultAction with text = CopyText; image = Some( Resource.Copy  ) };
    { DefaultAction with text = PasteText; image = Some( Resource.Paste ); enable = false };
    { DefaultAction with text = UndoText; image = Some( Resource.Undo ); enable = false };
    { DefaultAction with text = RedoText; image = Some( Resource.Redo ); enable = false }
    ]

let FormatActions =
  [ { DefaultAction with text = BoldText; image = Some( Resource.Bold ); toggle = true };
    { DefaultAction with text = ItalicText; image = Some( Resource.Italic ); toggle = true };
    { DefaultAction with text = UnderlineText; image = Some( Resource.Underline ); toggle = true };
    { DefaultAction with text = LeftText; image = Some( Resource.Left ); toggle = true ;}
    { DefaultAction with text = CenterText; image = Some( Resource.Center  ); toggle = true ;}
    { DefaultAction with text = RightText; image = Some( Resource.Right ); toggle = true ;}    
    ]

let ModeActions =
  [ { DefaultAction with text = TEditorText ;  image = Some (Resource.TEditor) ; toggle = true} ;
    { DefaultAction with text = RTFEditorText; image = Some (Resource.RTFEditor); toggle = true} ;
    { DefaultAction with text = PEditorText ;  image = Some (Resource.PEditor) ; toggle = true} 
    ]
