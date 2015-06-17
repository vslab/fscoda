FSEdit
======

FSEdit is a simple and experimental context-aware text editor, inspired by [CJEdit](https://www.hpi.uni-potsdam.de/hirschfeld/trac/Cop/wiki/JCopCJEdit) and implemented in [FSCoDa](https://github.com/vslab/fscoda), a Context-orietend Programming extension of F#. 

The editor supports three different execution modes: rich text editor, text editor and programming editor.
A context switch among the different modes changes the GUI of the editor, in particular, it offers different toolbars and menus.
In the programming mode a simple form of syntax highlighting is provided.

FSEdit can be built on Unix-based systems simply with

    ``make all``

command, assuming that mono and F# are installed.
