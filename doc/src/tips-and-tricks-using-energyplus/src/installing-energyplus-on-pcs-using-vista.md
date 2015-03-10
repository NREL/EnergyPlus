# Installing EnergyPlus on PC's using VISTA

*I am still having e+ installation problems of version xxx on windows vista. After double click on the exe file the installation routine starts normal, but then when "copying Visual Basic Runtime: C:\\Windows\\system32\\oleaut32.dll" the little papers signaling the copying keep flying but there is no installation progress any more. I already switched out the firewall, but as I am new to vista I don't know what else I could try?*

RESPONSE #1:

For another type of software, I had a problem getting the software to load on a Vista machine (very similar to what you are seeing). I don't recall exactly, but in the control panel there was an administrator area. In there the administrator control could be disabled and the software was then able to install. I then just enabled it again and everything worked as before. Hope this helps.

RESPONSE #2:

User Access Control of VISTA is causing the problem.

When using VISTA, you should treat it as a PC and server on the same machine, with an administrator sitting invisible.  Your hard drive is like a networked drive.  You must do as what the administrator allow you to do.  When you right click  a directory, you will see a 'share' option, select that and then security, you will see what your ID can do on that directory.  If you do not have 'full access', use the ID which has full access, or make your ID has 'full access'.  However, if you do not have the administrator right, you may not be able to do it.

So, you have to log in as an administrator to 'share' the disk with your ID.   This is what you usually has to do in an intranet on the network disk.

If you can make your user account as an administrator type, many installation problem may not occur.  You do not need to turn of the fire wall, etc. but to make sure your ID has the full control of the hard disk drive.  If you do not have the full control, you cannot create file folder on the drive, and the installation will fail.

If you turn off the User Access Control, the machine cannot ask you for the administrator permission, and therefore appears to be stopped.

If you have tried installing the program using another account, make sure that the directory is removed, or shared with your new ID.  Otherwise, any files in there cannot be replaced.  That may result in what you saw, because the oleaut32.dll  was not owned by you and the installer is searching for a solution, and this can take minutes.  Eventually, a pop up may show to ask you what to do.

 "copying Visual Basic Runtime:  C:\\Windows\\system32\\oleaut32.dll"  will stay until some time out mechanism kicks in.  If you are not an administrator, you normally cannot access the \\system32\\  directory.

When the files are copied from a CD_ROM during installation, the files will appear as 'Read only' on the hard disk.  When you run the simmulation, some of the  files must be modified.  The read only attribute on the files may abort a simulation run.  After the installation, you should verify that the .bat files are not all 'read only'.   You should also choose  'run as administrator' whenever possible when you run a EPlus application, so that the simulation program can modify directories and files.

Hope this will help.  Make yourself a standard user and administrator with a password, before you try to install again.

If you can switch off the fire wall, you should already have administration right, or administrator's permission.  If you did it with the administrator pop up, you account may not be an administrator type.