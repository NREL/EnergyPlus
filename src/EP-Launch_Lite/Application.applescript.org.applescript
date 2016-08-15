(* Application.applescript *)

(* ==== Event Handlers ==== *)

-- This event handler is called when any of the attached UI elements are clicked. One thing of note in the handling of clicking on stepper objects: you need to update the value of the text fields based on the value of the stepper in order to keep them in sync.

global weather_files
property selected_weather_file : ""

on clicked theObject
	set inputFileButtonClicked to false
	set launchEnergyPlusButtonClicked to false
	
	tell window "main"
		if theObject is equal to button "input" then
			set inputFileButtonClicked to true
			set launchEnergyPlusButtonClicked to false
		else if theObject is equal to button "launch" then
			set inputFileButtonClicked to false
			set launchEnergyPlusButtonClicked to true
			set scriptCommand to "runenergyplus \""
			set scriptCommand to scriptCommand & contents of text field "input file name" & "\""
			if selected_weather_file is not equal to "" then
				set scriptCommand to scriptCommand & " \"" & selected_weather_file & "\""
			end if
			tell application "Terminal"
				activate
				do script scriptCommand
			end tell
		end if
	end tell
	
	-- Setup the properties in the 'open panel'
	tell open panel
		if inputFileButtonClicked then
			set theTitle to "Select Input File"
			set theFileTypes to "idf, imf" as string
			set theDirectory to "/Applications/EnergyPlus/v2.0.0/examples" as string
			
			-- Convert the comma separated list of file type to an actual list
			set AppleScript's text item delimiters to ", "
			set theFileTypes to text items of theFileTypes
			set AppleScript's text item delimiters to ""
			
			set title to theTitle
			
			set prompt to "Open"
			set treat packages as directories to false
			set can choose directories to false
			set can choose files to true
			set allows multiple selection to false
		end if
	end tell
	
	if inputFileButtonClicked then
		-- Display the panel.
		-- Unlike the 'attached to' variant, the script does stop processing until the panel is finished.
		-- The 'in directory' and 'with file name' parameters are optional
		set theResult to display open panel in directory theDirectory for file types theFileTypes
		
		if theResult is 1 then
			set the pathNames to ((path names of open panel as list) as string)
			tell window "main"
				if inputFileButtonClicked then
					set contents of text field "input file name" to pathNames
				end if
			end tell
		end if
	end if
	
end clicked

-- This event handler is called when the text value of the attached text fields are changed. One thing of note in the handling of text fields with stepper objects: you need to update the value of the stepper based on the value of the text field in order to keep them in sync.
--
on action theObject
	tell window "main"
		if theObject is equal to popup button "weather popup" then
			if the title of popup button "weather popup" is not equal to "-- none --" then
				set selected_weather_file to the title of popup button "weather popup"
			else
				set selected_weather_file to ""
			end if
		end if
	end tell
end action

-- This event handler is called when the attached window is loaded from the nib file. It's a good place to set up the values of all of the UI elements based on the current drawer settings.
--
on awake from nib theObject
	tell theObject
	end tell
end awake from nib

on launched theObject
	show window "main"
	set weather_files to {"-- none --"}
	set x to ((path to applications folder) as string) & "EnergyPlus:v2.0.0:weatherdata"
	repeat with each_file in list folder (x)
		if each_file ends with ".epw" then
			set end of weather_files to contents of (each_file as string)
		end if
	end repeat
	
	tell window "main"
		delete every menu item of menu of popup button "weather popup"
		repeat with each_name in weather_files
			make new menu item at the end of menu of popup button "weather popup" with properties {title:each_name as string, enabled:true}
		end repeat
		--		set button "launch" to enabled
	end tell
end launched

