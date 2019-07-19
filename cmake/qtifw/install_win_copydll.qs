// Windows commands to be performed elevated: copy and register DLLs

function Component()
{
  Component.prototype.createOperations = function()
  {
    // call default implementation
    component.createOperations();

    // ... add custom operations

    var kernel = systemInfo.kernelType;
    // On Windows
    if( kernel == "winnt" ) {

      var systemArray = ["MSCOMCTL.OCX", "ComDlg32.OCX", "Msvcrtd.dll", "Dforrt.dll", "Gswdll32.dll", "Gsw32.exe", "Graph32.ocx", "MSINET.OCX", "Vsflex7L.ocx", "Msflxgrd.ocx"];
      var systemTargetDir = installer.environmentVariable("WINDIR");
      if( systemInfo.currentCpuArchitecture == "x86_64") {
        // This is where the 32-bit stuff is stored on 64-bit systems
        // (despite the name...)
        systemTargetDir += "\\SysWOW64\\";
      } else {
        // This is 32-bit on a 32-bit system
        systemTargetDir += "\\System32\\";
      }

      var regdll = systemTargetDir + "regsvr32.exe";

      for (i = 0; i < systemArray.length; i++) {
        var sourceFile = "@TargetDir@\\temp\\" + systemArray[i];
        var targetFile = systemTargetDir + systemArray[i];
        if (!installer.fileExists(targetFile)) {
          // Copy the DLL
          component.addElevatedOperation("Copy", sourceFile, targetFile);
          // Register it
          // Mind the "/s" flag which avoids displaying a [Yes/No] prompt
          // that you can't answer and making the installer freeze
          component.addElevatedOperation("Execute", regdll, "/s", targetFile,
             "UNDOEXECUTE", regdll, "/u", "/s", targetFile);
        }
      }
      // Delete this temp directory: use execute to avoid uninstall create
      // the opposite (= Mkdir), plus it doesn't delete an empty directory anyways and we use copy (not move) above...
      // component.addElevatedOperation("Rmdir", "@TargetDir@/temp");
      // /S = silent
      component.addElevatedOperation("Execute", "cmd" , "/C",  "rmdir", "/S", "@TargetDir@\\temp");

    }
  }
}
