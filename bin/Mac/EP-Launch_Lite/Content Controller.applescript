(* Content Controller.applescript *)

(* ==== Event Handlers ==== *)

on clicked theObject
	set contents of text field "Date Field" of drawer "Drawer" of window "Main" to (current date) as text
end clicked

on should open theObject
	set contents of text field "Date Field" of drawer "Drawer" of window "Main" to "should open"
	return false
end should open

on should close theObject
	set contents of text field "Date Field" of drawer "Drawer" of window "Main" to "should close"
	return true
end should close

on will open theObject
	set contents of text field "Date Field" of drawer "Drawer" of window "Main" to "will open"
end will open

on will resize theObject proposed size proposedSize
	log proposedSize as string
	set contents of text field "Date Field" of drawer "Drawer" of window "Main" to "will resize"
end will resize

on will close theObject
	set contents of text field "Date Field" of drawer "Drawer" of window "Main" to "will close"
end will close

on opened theObject
	set contents of text field "Date Field" of drawer "Drawer" of window "Main" to "opened"
end opened

on closed theObject
	set contents of text field "Date Field" of drawer "Drawer" of window "Main" to "closed"
end closed
