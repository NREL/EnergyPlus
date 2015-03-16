import sys

class idfFile(object):
    
    def __init__(self, fileName):
        
        # phase 0: read in lines of file
        lines = []
        with open(fileName, "r") as fo:
            lines = fo.readlines()
        
        # phases 1 and 2: remove comments and blank lines
        lines_a = []
        self.comments = []
        for line in lines:
            line_text = line.strip()
            this_line = ""
            if len(line_text) > 0:
                exclamation = line_text.find("!")
                if exclamation == -1:
                    this_line = line_text
                elif exclamation == 0:
                    this_line = ""
                    self.comments.append(line_text)
                elif exclamation > 0:
                    this_line = line_text[:exclamation]
                    self.comments.append(line_text[exclamation+1:])
                if not this_line == "":
                    lines_a.append(this_line)
        
        # intermediates: join entire array and re-split by semicolon
        idfDataJoined = ''.join(lines_a)
        idfObjectStrings = idfDataJoined.split(";")
        
        # phase 3: inspect each object and its fields
        objectDetails = []
        idfObjects = []
        for object in idfObjectStrings:
            tokens = object.split(",")
            niceObject = [ t.strip() for t in tokens ]
            if len(niceObject) == 1:
                if niceObject[0] == "":
                    continue
            objectDetails.append(niceObject)
            idfObjects.append(idfObject(niceObject))    
        
        # now store it on the class
        self.idfObjects = idfObjects

class idfObject(object):

    def __init__(self, tokens):
        self.objectName = tokens[0]
        self.fields = tokens[1:]
        
    def objectString(self):
        if len(self.fields) == 0:
            s = self.objectName + ";\n"
        else:
            s = self.objectName + ",\n"
            for field in self.fields[:-1]:
                s += "  " + field + ",\n"
            s += "  " + self.fields[-1] + ";\n"
        return s
        
    def printObject(self):
        print self.objectString()
        
    def writeObject(self, fileObject):
        fileObject.write(self.objectString())
    
