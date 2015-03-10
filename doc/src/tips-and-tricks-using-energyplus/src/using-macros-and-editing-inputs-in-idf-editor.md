# Using Macros and Editing Inputs in IDF Editor 

*How can I use macros, and continue to edit my input in IDF editor?*

*(Using or ignoring macros in the IDF editor is a potential Enhancement List item.)*

#. Separate files into "IDF editable" and "macro" (actually, the AbsorptionChiller_Macro.imf example file shows a little of this but it doesn't really use macros). For the pieces you think you'd like to manipulate in the IDF editor, call them with extension IDF. For the others, they would be IMF and the master file would be IMF with "includes" of your IDF pieces.
#. Use the expanded IDF (extension epmidf) file for your IDF editor changes and then run it from there.