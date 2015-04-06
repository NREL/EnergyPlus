# python 2/3 compatibility imports
from __future__ import absolute_import
from __future__ import unicode_literals
from __future__ import print_function
import io


class iddFile(object):
    def __init__(self, file_name):

        # phase 0: read in lines of file
        with io.open(file_name, 'r', encoding='latin-1') as fo:
            lines = fo.readlines()

        # phases 1 and 2: remove comments and blank lines
        lines_a = []
        self.comments = []
        line_num = 0
        for line in lines:
            line_num += 1
            line_text = line.strip()
            this_line = ''
            if len(line_text) > 0:
                exclamation = line_text.find('!')
                slash = line_text.find("\\")
                comment_point = -1
                if exclamation == -1 and slash == -1:
                    comment_point = -1
                elif exclamation > -1 and slash == -1:
                    comment_point = exclamation
                elif exclamation == -1 and slash > -1:
                    comment_point = slash
                elif exclamation > -1 and slash > -1:
                    comment_point = min(slash, exclamation)
                if comment_point == -1:
                    this_line = line_text
                elif comment_point == 0:
                    this_line = ''
                    self.comments.append(line_text)
                elif comment_point > 0:
                    this_line = line_text[:comment_point]
                    self.comments.append(line_text[comment_point + 1:])
                if not this_line == '':
                    lines_a.append(this_line)

        # intermediates: join entire array and re-split by semicolon
        idd_data_joined = ''.join(lines_a)
        idd_object_strings = idd_data_joined.split(';')

        # phase 3: inspect each object and get it's name
        self.iddObjects = []
        for idf_object in idd_object_strings:
            tokens = idf_object.split(',')
            nice_object = [t.strip() for t in tokens]
            if len(nice_object) == 1:
                if nice_object[0] == '':
                    continue
            self.iddObjects.append(nice_object[0].upper())

