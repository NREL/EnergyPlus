# IDF Version Converter / Transition File Lists

Both the console program and the IDF-Version Updater (see) can use a text file of file names to perform the transitions. The file is a very simple list of file names:

~~~~~~~~~~~~~~~~~~~~
    FileName1
    FileName2
~~~~~~~~~~~~~~~~~~~~

But you must include the path name in the file if it is not in the same folder as the transition set of programs:

~~~~~~~~~~~~~~~~~~~~
    C:\Myfiles\abc.idf
    C:\My Working Files\abcdef.idf
    D:\OtherFiles\xxxxxyyyyy.idf
    Etc
~~~~~~~~~~~~~~~~~~~~

Note that the files need not be in the same folder. And, if you use the IDF Version Updater app, they need not all be the same version! If you use the straight batch files form the CMD line, all files in this list must be the same starting version and you will need to select that version when you run the batch file. List files have the implicit transition options seen in the following section.
