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

      // OCX: needs copy AND registration
      // DLL and exe: just copy
      // Note: the order of ocx/dll in the array matters for registering the ocx!
      // To be safe and not depend on order, let's be smarter: we copy
      // everything in one pass, then we register the ones that need to be in a
      // second pass
      var systemArray = [
        "MSCOMCTL.OCX", "ComDlg32.OCX", "Msvcrtd.dll", "Dforrt.dll",
        "Gswdll32.dll", "Gsw32.exe", "Graph32.ocx",
        "MSINET.OCX", "Vsflex7L.ocx", "Msflxgrd.ocx"
      ];

      var systemTargetDir = installer.environmentVariable("SystemRoot");
      // Note: all dlls in ./bin/System are 32-bits
      if( systemInfo.currentCpuArchitecture == "x86_64") {
        // This is where the 32-bit stuff is stored on 64-bit systems
        // (despite the name...)
        systemTargetDir += "\\SysWOW64\\";
      } else {
        // This is 32-bit on a 32-bit system
        systemTargetDir += "\\System32\\";
      }

      var regdll = systemTargetDir + "regsvr32.exe";

      // Store ocx to be registered
      var dllsToReg = [];
      for (i = 0; i < systemArray.length; i++) {
        var sourceFile = "@TargetDir@\\temp\\" + systemArray[i];
        var targetFile = systemTargetDir + systemArray[i];
        if (!installer.fileExists(targetFile)) {
          console.log("Copying DLL: " + targetFile);
          // Copy the DLL (includes reverting on uninstall)
          // component.addElevatedOperation("Copy", sourceFile, targetFile);

          // Copy without deleting on uninstall (technically the "/Y"
          // (=overwrite with no prompt) isn't needed since we tested target
          // didn't exist already
          component.addElevatedOperation("Execute", "cmd", "/C", "copy", sourceFile, targetFile, "/Y");
        }
        // Register it: Only for "OCX"
        // On some systems these files may be present but not properly registered, so always register here
        // If it's a .ocx (case insensitive), we save it to be registered
        if (systemArray[i].toLowerCase().indexOf(".ocx") !== -1) {
          dllsToReg.push(targetFile);
        }
      }

      for (i = 0; i < dllsToReg.length; i++) {
        targetFile = dllsToReg[i];
        // Mind the "/s" flag which avoids displaying a [Yes/No] prompt
        // that you can't answer and making the installer freeze
        console.log("Registering DLL: " + [regdll, "/s", targetFile].join(" "));
        component.addElevatedOperation("Execute", regdll, "/s", targetFile);
          // We do not undo
          // "UNDOEXECUTE", regdll, "/u", "/s", targetFile);

      }
      // Delete this temp directory: use execute to avoid uninstall create
      // the opposite (= Mkdir), plus it doesn't delete an empty directory anyways and we use copy (not move) above...
      // component.addElevatedOperation("Rmdir", "@TargetDir@/temp");
      // /S = recursive, /Q = quiet
      component.addElevatedOperation("Execute", "cmd" , "/C",  "rmdir", "/S", "/Q", "@TargetDir@\\temp");

    }
  }
}
