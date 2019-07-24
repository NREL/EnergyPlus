# Base class of model auditors and exceptions


class BadModel(Exception):
    pass


class Auditor:
    def __init__(self, model):
        self.model = model # The model
        self.json = {} # JSON output dictionary

    def audit(self, **kwargs):
        return True
