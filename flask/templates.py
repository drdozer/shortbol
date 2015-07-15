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

        self.InlineRange = """InlineRange => Range
   orientation = <SBOL:inline>"""

        self.ReverseCompRange = """ReverseCompRange => Range
   orientation = <SBOL:reverseComplement>"""

        self.Inline = """Inline(s,e) => SequenceAnnotation
   location : InlineRange
      start = s
      end = e"""

        self.ReverseComp = """RevComp(s,e) => SequenceAnnotation
   location: ReverseCompRange
      start = s
      end = e"""

        self.Driver = """Driver(x) => Participation
   participant = x
   role = <SBOL:driver>"""

        self.Driven = """Driven(x) => Participation
   participant = x
   role = <SBOL:driven>"""

        self.Repressor = """Repressor(x) => Participation
   participant = x
   role = <SBOL:Repressor>"""

        self.Repressed  = """Repressed(x) => Participation
   participant = x
   role = <SBOL:Repressed>"""

        self.Activated = """Activated(x) => Participation
   participant = x
   role = <SBOL:activated>"""

        self.Activator  = """Activator(x) => Participation
   participant = x
   role = <SBOL:activator>"""

        self.TranscriptionTemplate = """TranscriptionTemplate(x) => Participation
   participant = x
   role =<SBOL:Transcriptional_template>"""

        self.TranscriptionProduct = """TranscriptionProduct(x) => Participation
   participant = x
   role = <SBOL:Transcriptional_product>"""

        self.TranslationTemplate = """TranslationTemplate(x) => Participation
   participant = x
   role = <SBOL:Translation_template>"""

        self.TranslationProduct = """TranslationProduct(x) => Participation
   participant = x
   role = <SBOL:Translation_product>"""

        self.Inhibitor = """Inhibitor(x) => Participation
   participant = x
   role = <SBOL:http://identifiers.org/biomodels.sbo/SBO:0000020>"""

        self.Inhibited = """Inhibited(x) => Participation
   participant = x
   role = <SBOL:http://identifiers.org/biomodels.sbo/SBO:0000598>"""

        self.Transcription = """Transcription(cds, cds_rna) => Interaction
   type = <SBOL:Transcription>
   TranscriptionTemplate(cds)
   TranscriptionProduct(cds_rna)"""

        self.Translation = """Translation(cds_rna, protein) => Interaction
   type = <SBOL:Translation>
   TranslationTemplate(cds_rna)
   TranslationProduct(protein)"""

        self.ProteinProduction = """ProteinProduction(cds, cds_rna, protein) => Interaction*
   Transcription(cds, cds_rna)
   Translation(cds_rna, protein)"""

        self.activates = """activates(tf, promotor) => Interaction
   type = <SBOL:Activates>
   Activator(tf)
   Activated(promotor)"""

        self.drives = """drives(promotor,cds) => Interaction
   type = <SBOL:Drives>
   Driver(promotor)
   Driven(cds)"""

        self.represses = """represses(a,b) => Interaction
   type = <SBOL:Represses>
   Repressor(a)
   Repressed(b)"""

        self.inhibits = """inhibits(tf,promotor) => Interaction
   type = <SBOL:http://identifiers.org/biomodels.sbo/SBO:0000169> #inhibits
   Inhibitor(tf)
   Inhibited(promotor)"""

        self.InverterModule = """InverterModule => ModuleDefinition
   role = <SBOL:http://parts.igem.org/cgi/partsdb/pgroup.cgi?pgroup=inverter>"""

        self.ToggleModule = """ToggleModule => ModuleDefinition
   role = <SBOL:http://sbolstandard.org/example/module_role/toggle_switch>"""

        self.public = """public => component : Component
   access = <SBOL:Public>"""

        self.public_i = """public_input => functionalComponent: FunctionalComponent
   access = <SBOL:Public>
   direction = <SBOL:input>"""

        self.public_o = """public_output => functionalComponent: FunctionalComponent
   access = <SBOL:Public>
   direction = <SBOL:output>"""

        self.public_io = """public_io => functionalComponent: FunctionalComponent
   access = <SBOL:Public>
   direction = <SBOL:inout>"""

        self.private = """private => component : Component
   access = <SBOL:Private>"""

        self.private_i = """private_input => functionalComponent: FunctionalComponent
   access = <SBOL:Private>
   direction = <SBOL:input>"""

        self.private_o = """private_output => functionalComponent: FunctionalComponent
   access = <SBOL:Private>
   direction = <SBOL:output>"""

        self.private_io = """private_io => functionalComponent: FunctionalComponent
   access = <SBOL:Private>
   direction = <SBOL:inout>"""


