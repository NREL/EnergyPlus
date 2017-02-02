# python 2/3 compatibility imports
from __future__ import absolute_import
from __future__ import unicode_literals
from __future__ import print_function
import io


class idfFile(object):
    def __init__(self, file_name):

        # phase 0: read in lines of file
        with io.open(file_name, 'r', encoding='latin-1') as fo:
            lines = fo.readlines()

        # phases 1 and 2: remove comments and blank lines
        lines_a = []
        self.comments = []
        for line in lines:
            line_text = line.strip()
            this_line = ''
            if len(line_text) > 0:
                exclamation = line_text.find('!')
                if exclamation == -1:
                    this_line = line_text
                elif exclamation == 0:
                    this_line = ''
                    self.comments.append(line_text)
                elif exclamation > 0:
                    this_line = line_text[:exclamation]
                    self.comments.append(line_text[exclamation + 1:])
                if not this_line == '':
                    lines_a.append(this_line)

        # intermediates: join entire array and re-split by semicolon
        idf_data_joined = ''.join(lines_a)
        idf_object_strings = idf_data_joined.split(';')

        # phase 3: inspect each object and its fields
        object_details = []
        idf_objects = []
        for idf_object in idf_object_strings:
            tokens = idf_object.split(',')
            nice_object = [t.strip() for t in tokens]
            if len(nice_object) == 1:
                if nice_object[0] == '':
                    continue
            object_details.append(nice_object)
            idf_objects.append(idfObject(nice_object))

            # now store it on the class
        self.idf_objects = idf_objects


class idfObject(object):
    def __init__(self, tokens):
        self.objectName = tokens[0]
        self.fields = tokens[1:]

    def __str__(self):
        if len(self.fields) == 0:
            s = self.objectName + ";\n"
        else:
            s = self.objectName + ",\n"
            for field in self.fields[:-1]:
                s += "  " + field + ",\n"
            s += "  " + self.fields[-1] + ";\n"
        return s
