# Quick Procedure Outline For Making Code Changes to EnergyPlus

Some of the steps in this section are primarily applicable to developers who are part of the "EnergyPlus Team".  However, these steps should also be followed as you develop a module or other piece to submit to the EnergyPlus Team for inclusion in an EnergyPlus release.

**Write a New Feature Proposal (often called NFP) for discussion at a bi-weekly conference call.  Based on that discussion, update the NFP. Out of team developers: use the NFP format to help formulate your submission documentation to the EnergyPlus Team. The NFP format is shown in Appendix F. The sections of the NFP format are shown in Table 1. Sections of a New Feature Proposal.**

Get the relevant files for your development. Team developers can check out files from StarTeam.

Energy+.idd and Featurechanges.csv are in the 'Release' Folder. This folder also contains the "Rules" spreadsheet and "Report Variables" files.

ExampleFiles.xls, ExampleFilesDoc.txt, and baseline or relevant IDF files from 'Test Files - Utilities\\InternalTests\\InputFiles'

F90 files from the 'SourceCode' folder.

Documents (InputOutputReference.doc, EngineeringReference.doc, OutputDetailsAndExamples.doc, etc.) from the 'External Documentation\\Documentation Sources' folder.

Following proper procedures (*e.g., Object naming conventions are specified earlier in this document*) make your object changes to the Energy+.idd and relevant IDF files.  If your IDD modifications make changes for existing objects, you must determine if you need to add to the "Rules" spreadsheet so that the transition program can be made for existing IDF files.  Likewise, if you change existing report variable names, you must update the "Report Variables" file. Note that the ExampleFiles.xls has some guidance on the contents of new test suite files. Create or change existing IDF files for your feature.

Make code changes to F90 files for subroutines, GetInput, Sim, Report or create your own module following the Programming Standards and Programming Templates.  Programming Templates are available in Appendix D.  Programming Standards is a separate document.

Compile and run in debug mode to track errors.

Test making sample runs; review summary and time step reports to identify issues. Test many features of your module even if you are not including all in the Test Suite IDF file. Go back to Step 4 as necessary. Note some of the issues in the "Important Rules for Developers".

When complete, run full test suite to make sure there are no crashes or unexpected changes in other files.

Run Reverse DD for your featured files – making sure the results *exactly* match.

Update relevant portions of documents: InputOutputReference.doc, EngineeringReference.doc, OutputDetailsAndExamples.doc, etc.  Only excerpted portions of the document should be sent forward for review and final inclusion in the whole document. Depending on the changes, it may be better to "track changes" in the document or give instructions for inclusion in the whole document. Send these documents up the review chain as appropriate.  **Appendix C also has some information about formatting documents.**

Procedure on checking in files changes from time to time.  Currently, all source code files are kept locked and your code may go through another reivew before you are allowed to check in.  **Follow procedures in Appendix B as well about submitting your feature – many parts are repeated in this section.**

Check in modified or new files. If changes have been made to the original checked out file, you must carefully merge your changes into the file – this also may necessitate you repeating your test runs. Usually, your featurechanges modification will be very simple and be the last line in that file.

Send email to the team notifying them of the new feature/changed feature/defect fix and what files were changed/added/etc.

Incorporate any feedback after checkin.

Use **Appendix G** to submit information, if applicable, about your feature.