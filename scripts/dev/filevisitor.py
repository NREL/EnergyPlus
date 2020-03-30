import glob


class FileVisitor:
    def __init__(self, extensions = None):
        self.visited_files = []
        if extensions == None:
            self.extensions = ['cc', 'cpp', 'c', 'hh', 'hpp', 'h']
        else:
            self.extensions = extensions

    def files(self, path):
        results = []
        for ext in self.extensions:
            results.extend(glob.glob(path+'**/*.'+ext, recursive=True))
        return results

    def visit_file(self, filepath):
        pass

    def error(self, file, line_number, mesg):
        pass

    def info(self, file, line_number, mesg):
        pass

    def visit(self, path):
        for file in self.files(path):
            self.visit_file(file)
            self.visited_files.append(file)

    def readtext(self, filepath):
        fp = open(filepath, 'r', encoding='utf-8')
        try:
            txt = fp.read()
        except UnicodeDecodeError as exc:
            self.error(filepath, 0, 'UnicodeDecodeError: '+ str(exc))
            txt = None
        except Exception as exc:
            self.error(filepath, 0, 'Exception: '+ str(exc))
            txt = None
        fp.close()
        return txt



