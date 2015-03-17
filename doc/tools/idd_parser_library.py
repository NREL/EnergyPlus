import sys

class iddFile(object):
    
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
                slash = line_text.find("\\")
                comment_point = -1
                if exclamation == -1 and slash == -1:
                    comment_point = -1
                elif exclamation > -1 and slash == -1:
                    comment_point = exclamation
                elif exclamation == -1 and slash > -1:
                    comment_point = slash
                elif exclamation > -1 and slash > -1:
                    comment_point = min(comment_point, exclamation)
                if slash > -1:
                    comment_point = min(comment_point, slash)
                if comment_point == -1:
                    this_line = line_text
                elif comment_point == 0:
                    this_line = ""
                    self.comments.append(line_text)
                elif comment_point > 0:
                    this_line = line_text[:comment_point]
                    self.comments.append(line_text[comment_point+1:])
                if not this_line == "":
                    lines_a.append(this_line)
        
        # intermediates: join entire array and re-split by semicolon
        iddDataJoined = ''.join(lines_a)
        iddObjectStrings = iddDataJoined.split(";")
        
        # phase 3: inspect each object and its fields
        self.iddObjects = []
        for object in iddObjectStrings:
            tokens = object.split(",")
            niceObject = [ t.strip() for t in tokens ]
            if len(niceObject) == 1:
                if niceObject[0] == "":
                    continue
            self.iddObjects.append(niceObject[0].upper())

