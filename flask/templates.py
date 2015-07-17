__author__ = 'chris'

from collections import OrderedDict

class Templates:

    keys = OrderedDict()

    def __init__(self):
        pass

    def getExamples(self):
        return self.keys

class ComponentDefinitions(Templates):

    keys = OrderedDict()

    def __init__(self):

        self.keys['DNAComponent'] = """DNAComponent => ComponentDefinition
   type = <SBOL:DNA>"""

        self.keys['RNAComponent'] = """RNAComponent => ComponentDefinition
   type = <SBOL:RNA>"""

        self.keys['ProteinComponent'] = """ProteinComponent => ComponentDefinition
   type = <SBOL:Protein>"""

        self.keys['SmallMoleculeComponent'] = """SmallMoleculeComponent => ComponentDefinition
   type = <SBOL:SmallMolecule>"""

        self.keys['ComplexComponent'] = """ComplexComponent => ComponentDefinition
   type = <SBOL:Complex>"""

        self.keys['Promotor'] = """Promoter => DNAComponent
   role = <SBOL:Promoter>"""

        self.keys['RBS'] = """RBS => DNAComponent
   role = <SBOL:RBS>"""

        self.keys['CDS'] = """CDS => DNAComponent
   role = <SBOL:CDS>"""

        self.keys['Terminator'] = """Terminator => DNAComponent
   role = <SBOL:Terminator>"""

        self.keys['CDS_RNA'] = """CDS_RNA => RNAComponent
   role = <SBOL:CDS>"""

        self.keys['Gene'] = """Gene => DNAComponent
   role = <SBOL:Gene>"""

        self.keys['Operator'] = """Operator => DNAComponent
   role = <SBOL:Operator>"""

        self.keys['EngineeredGene'] = """EngineeredGene => DNAComponent
   role = <SBOL:EngineeredGene>"""

        self.keys['MRNA'] = """MRNA => RNAComponent
   role = <SBOL:MRNA>"""

        self.keys['Effector'] = """Effector => SmallMoleculeComponent
   role = <SBOL:Effector>"""



class SequenceAnnotations(Templates):

    keys = OrderedDict()

    def __init__(self):

        self.keys['InlineRange'] = """InlineRange => Range
   orientation = <SBOL:inline>"""

        self.keys['ReverseCompRange'] = """ReverseCompRange => Range
   orientation = <SBOL:reverseComplement>"""

        self.keys['Inline'] = """Inline(s,e) => SequenceAnnotation
   location : InlineRange
      start = s
      end = e"""

        self.keys['ReverseComp'] = """ReverseComp(s,e) => SequenceAnnotation
   location: ReverseCompRange
      start = s
      end = e"""

class Participations(Templates):

    keys = OrderedDict()


    def __init__(self):

        self.keys['Driver'] = """Driver(x) => Participation
   participant = x
   role = <SBOL:driver>"""

        self.keys['Driven'] = """Driven(x) => Participation
   participant = x
   role = <SBOL:driven>"""

        self.keys['Repressor'] = """Repressor(x) => Participation
   participant = x
   role = <SBOL:Repressor>"""

        self.keys['Repressed']  = """Repressed(x) => Participation
   participant = x
   role = <SBOL:Repressed>"""

        self.keys['Activated'] = """Activated(x) => Participation
   participant = x
   role = <SBOL:activated>"""

        self.keys['Activator']  = """Activator(x) => Participation
   participant = x
   role = <SBOL:activator>"""

        self.keys['TranscriptionTemplate'] = """TranscriptionTemplate(x) => Participation
   participant = x
   role =<SBOL:Transcriptional_template>"""

        self.keys['TranscriptionProduct'] = """TranscriptionProduct(x) => Participation
   participant = x
   role = <SBOL:Transcriptional_product>"""

        self.keys['TranslationTemplate'] = """TranslationTemplate(x) => Participation
   participant = x
   role = <SBOL:Translation_template>"""

        self.keys['TranslationProduct'] = """TranslationProduct(x) => Participation
   participant = x
   role = <SBOL:Translation_product>"""

        self.keys['Inhibitor'] = """Inhibitor(x) => Participation
   participant = x
   role = <SBOL:http://identifiers.org/biomodels.sbo/SBO:0000020>"""

        self.keys['Inhibited'] = """Inhibited(x) => Participation
   participant = x
   role = <SBOL:http://identifiers.org/biomodels.sbo/SBO:0000598>"""

class Interactions(Templates):

    keys = OrderedDict()

    def __init__(self):

        self.keys['Transcription'] = """Transcription(cds, cds_rna) => Interaction
   type = <SBOL:Transcription>
   TranscriptionTemplate(cds)
   TranscriptionProduct(cds_rna)"""

        self.keys['Translation'] = """Translation(cds_rna, protein) => Interaction
   type = <SBOL:Translation>
   TranslationTemplate(cds_rna)
   TranslationProduct(protein)"""

        self.keys['ProteinProduction'] = """ProteinProduction(cds, cds_rna, protein) => Interaction*
   Transcription(cds, cds_rna)
   Translation(cds_rna, protein)"""

        self.keys['activates'] = """activates(tf, promotor) => Interaction
   type = <SBOL:Activates>
   Activator(tf)
   Activated(promotor)"""

        self.keys['drives'] = """drives(promotor,cds) => Interaction
   type = <SBOL:Drives>
   Driver(promotor)
   Driven(cds)"""

        self.keys['represses'] = """represses(a,b) => Interaction
   type = <SBOL:Represses>
   Repressor(a)
   Repressed(b)"""

        self.keys['inhibits'] = """inhibits(tf,promotor) => Interaction
   type = <SBOL:http://identifiers.org/biomodels.sbo/SBO:0000169> #inhibits
   Inhibitor(tf)
   Inhibited(promotor)"""

class ModuleDefinitions(Templates):

    keys = OrderedDict()

    def __init__(self):

        self.keys['InverterModule'] = """InverterModule => ModuleDefinition
   role = <SBOL:http://parts.igem.org/cgi/partsdb/pgroup.cgi?pgroup=inverter>"""

        self.keys['ToggleModule'] = """ToggleModule => ModuleDefinition
   role = <SBOL:http://sbolstandard.org/example/module_role/toggle_switch>"""

class PropertyConstructors(Templates):

    keys = OrderedDict()

    def __init__(self):

        self.keys['public'] = """public => component : Component
   access = <SBOL:Public>"""

        self.keys['public_input'] = """public_input => functionalComponent: FunctionalComponent
   access = <SBOL:Public>
   direction = <SBOL:input>"""

        self.keys['public_output'] = """public_output => functionalComponent: FunctionalComponent
   access = <SBOL:Public>
   direction = <SBOL:output>"""

        self.keys['public_io'] = """public_io => functionalComponent: FunctionalComponent
   access = <SBOL:Public>
   direction = <SBOL:inout>"""

        self.keys['private'] = """private => component : Component
   access = <SBOL:Private>"""

        self.keys['private_input'] = """private_input => functionalComponent: FunctionalComponent
   access = <SBOL:Private>
   direction = <SBOL:input>"""

        self.keys['private_output'] = """private_output => functionalComponent: FunctionalComponent
   access = <SBOL:Private>
   direction = <SBOL:output>"""

        self.keys['private_io'] = """private_io => functionalComponent: FunctionalComponent
   access = <SBOL:Private>
   direction = <SBOL:inout>"""


class Sequences(Templates):

    keys = OrderedDict()

    def __init__(self):

        self.keys['DNASequence'] = """DNASequence(x) => Sequence
   encoding = <SBOL:IUPACDNA>
   elements = x"""
        self.keys['ProteinSequence'] = """ProteinSequence(x) => Sequence
   encoding = <SBOL:IUPACProtein>
   elements = x"""

        self.keys['SmallMoleculeSequence'] = """SmallMoleculeSequence(x) => Sequence
   encoding = <SBOL:SMILES>
   elements = x"""

class MapsTos(Templates):

    keys = OrderedDict()

    

