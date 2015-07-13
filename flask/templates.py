__author__ = 'chris'

class Templates:

    def __init__(self):
        self.DNASequence = """DNASequence(x) => Sequence
   encoding = <SBOL:IUPACDNA>
   elements = x
"""
        self.DNAComponent = """DNAComponent => ComponentDefinition
   type = <SBOL:DNA>"""

        self.RNAComponent = """RNAComponent => ComponentDefinition
   type = <SBOL:RNA>"""

        self.ProteinComponent = """ProteinComponent => ComponentDefinition
   type = <SBOL:Protein>"""

        self.Promotor = """Promoter => DNAComponent
   role = <SBOL:Promoter>"""

        self.RBS = """RBS => DNAComponent
   role = <SBOL:RBS>"""

        self.CDS = """CDS => DNAComponent
   role = <SBOL:CDS>"""

        self.Terminator = """Terminator => DNAComponent
   role = <SBOL:Terminator>"""

        self.CDS_RNA = """CDS_RNA => RNAComponent
   role = <SBOL:CDS>"""