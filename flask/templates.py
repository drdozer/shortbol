s__author__ = 'chris'

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

        self.keys['Promoter'] = """Promoter => DNAComponent
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
   orientation = <SBOL:InlineRange>"""

        self.keys['ReverseCompRange'] = """ReverseCompRange => Range
   orientation = <SBOL:ReverseComplement>"""

        self.keys['InlinePosition'] = """InlinePosition(s,e) => SequenceAnnotation
   location : InlineRange
      start = s
      end = e"""

        self.keys['ReverseCompPosition'] = """ReverseCompPosition(s,e) => SequenceAnnotation
   location: ReverseCompRange
      start = s
      end = e"""

class Participations(Templates):

    keys = OrderedDict()


    def __init__(self):

        self.keys['Driver'] = """Driver(x) => Participation
   participant = x
   role = <SBOL:Driver>"""

        self.keys['Driven'] = """Driven(x) => Participation
   participant = x
   role = <SBOL:Driven>"""

        self.keys['Repressor'] = """Repressor(x) => Participation
   participant = x
   role = <SBOL:Repressor>"""

        self.keys['Repressed']  = """Repressed(x) => Participation
   participant = x
   role = <SBOL:Repressed>"""

        self.keys['Activated'] = """Activated(x) => Participation
   participant = x
   role = <SBOL:Activated>"""

        self.keys['Activator']  = """Activator(x) => Participation
   participant = x
   role = <SBOL:Activator>"""

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
   role = <SBOL:Inhibitor> #http://identifiers.org/biomodels.sbo/SBO:0000020>"""

        self.keys['Inhibited'] = """Inhibited(x) => Participation
   participant = x
   role = <SBOL:Inhibited> #http://identifiers.org/biomodels.sbo/SBO:0000598>"""

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

        self.keys['activates'] = """activates(tf, promoter) => Interaction
   type = <SBOL:Activates>
   Activator(tf)
   Activated(promoter)"""

        self.keys['drives'] = """drives(promoter,cds) => Interaction
   type = <SBOL:Drives>
   Driver(promoter)
   Driven(cds)"""

        self.keys['represses'] = """represses(a,b) => Interaction
   type = <SBOL:Represses>
   Repressor(a)
   Repressed(b)"""

        self.keys['inhibits'] = """inhibits(tf,promoter) => Interaction
   type = <SBOL:Inhibits> #http://identifiers.org/biomodels.sbo/SBO:0000169
   Inhibitor(tf)
   Inhibited(promoter)"""

class ModuleDefinitions(Templates):

    keys = OrderedDict()

    def __init__(self):

        self.keys['InverterModule'] = """InverterModule => ModuleDefinition
   role = <SBOL:InverterModule> #http://parts.igem.org/cgi/partsdb/pgroup.cgi?pgroup=inverter>"""

        self.keys['ToggleModule'] = """ToggleModule => ModuleDefinition
   role = <SBOL:ToggleModule> #http://sbolstandard.org/example/module_role/toggle_switch>"""

class PropertyConstructors(Templates):

    keys = OrderedDict()

    def __init__(self):

        self.keys['public'] = """public => component : Component
   access = <SBOL:Public>"""

        self.keys['public_input'] = """public_input => functionalComponent: FunctionalComponent
   access = <SBOL:Public>
   direction = <SBOL:Input>"""

        self.keys['public_output'] = """public_output => functionalComponent: FunctionalComponent
   access = <SBOL:Public>
   direction = <SBOL:Output>"""

        self.keys['public_io'] = """public_io => functionalComponent: FunctionalComponent
   access = <SBOL:Public>
   direction = <SBOL:Inout>"""

        self.keys['private'] = """private => component : Component
   access = <SBOL:Private>"""

        self.keys['private_input'] = """private_input => functionalComponent: FunctionalComponent
   access = <SBOL:Private>
   direction = <SBOL:Input>"""

        self.keys['private_output'] = """private_output => functionalComponent: FunctionalComponent
   access = <SBOL:Private>
   direction = <SBOL:Output>"""

        self.keys['private_io'] = """private_io => functionalComponent: FunctionalComponent
   access = <SBOL:Private>
   direction = <SBOL:Inout>"""


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

    def __init__(self):

        self.keys["implements"] = """replaces(l,r) => MapsTo
   refinement = <SBOL:UseLocal>
   local = l
   remote = r"""

        self.keys["uses"] = """uses(l,r) => MapsTo
   refinement = <SBOL:UseRemote>
   local = l
   remote = r"""

        self.keys["is_equal_to"] = """is_equal_to(l,r) => MapsTo
   refinement = <SBOL:VerifyIdentical>
   local = l
   remote = r"""

        self.keys["merge"] = """merge(l,r) => MapsTo
   refinement = <SBOL:Merge>
   local = l
   remote = r"""

class Models(Templates):

    keys = OrderedDict()

    def __init__(self):

        self.keys["SBMLModel"] = """SBMLModel => Model
   language = <SBOL:SBMLModel>  #http://identifiers.org/edam/format_2585"""

        self.keys["CellMLModel"] = """CellMLModel => Model
   language = <SBOL:CellMLModel>  #http://identifiers.org/edam/format_3240"""

        self.keys["BioPAXModel"] = """BioPAXModel => Model
   language = <SBOL:BioPAXModel>  #http://identifiers.org/edam/format_3156"""


        self.keys["ContinuousSBMLModel"] = """ContinuousSBMLModel => Model
   language = <SBOL:SBMLModel>  #http://identifiers.org/edam/format_2585
   framework = <SBOL:Continuous> #http://identifiers.org/biomodels.sbo/SBO:0000062"""

        self.keys["ContinuousCellMLModel"] = """ContinuousCellMLModel => Model
   language = <SBOL:CellMLModel>  #http://identifiers.org/edam/format_3240
   framework = <SBOL:Continuous> #http://identifiers.org/biomodels.sbo/SBO:0000062"""

        self.keys["ContinuousBioPAXModel"] = """ContinuousBioPAXModel => Model
   language = <SBOL:BioPAXModel>  #http://identifiers.org/edam/format_3156
   framework = <SBOL:Continuous> #http://identifiers.org/biomodels.sbo/SBO:0000062"""



class ExampleCode():

    def __init__(self):

        self.code = """
import genomic_generics

##################### Define the sequences ##############################

BBa_J61101_seq : DNASequence("aaagacaggacc")

BBa_J61120_seq : DNASequence("aaagacaggacc")

BBa_E0040_seq : DNASequence({
atgcgtaaaggagaagaacttttcactggagttgtcccaattcttgttgaattagatggtgatgttaatgg
gcacaaattttctgtcagtggagagggtgaaggtgatgcaacatacggaaaacttacccttaaatttatttgcactactggaaaac
tacctgttccatggccaacacttgtcactactttcggttatggtgttcaatgctttgcgagatacccagatcatatgaaacagcat
gactttttcaagagtgccatgcccgaaggttatgtacaggaaagaactatatttttcaaagatgacgggaactacaagacacgtgc
tgaagtcaagtttgaaggtgatacccttgttaatagaatcgagttaaaaggtattgattttaaagaagatggaaacattcttggac
acaaattggaatacaactataactcacacaatgtatacatcatggcagacaaacaaaagaatggaatcaaagttaacttcaaaatt
agacacaacattgaagatggaagcgttcaactagcagaccattatcaacaaaatactccaattggcgatggccctgtccttttacc
agacaaccattacctgtccacacaatctgccctttcgaaagatcccaacgaaaagagagaccacatggtccttcttgagtttgtaa
cagctgctgggattacacatggcatggatgaactatacaaataataa
})

ECK120033736_seq : DNASequence("ttcagccaaaaaacttaagaccgccggtcttgtccactaccttgcagtaatgcggtggacaggatcggcggttttcttttctcttctcaa")

BBa_R0010_seq : DNASequence("tccctatcagtgatagagattgacatccctatcagtgatagagatactgagcac")

BBa_R0040_seq : DNASequence("tccctatcagtgatagagattgacatccctatcagtgatagagatactgagcac")

BBa_J61130_seq : DNASequence("aaagaaacgaca")

BBa_C0040_seq : DNASequence(
{
atgtccagattagataaaagtaaagtgattaacagcgcattagagctgcttaatgaggtcggaatcgaagg
tttaacaacccgtaaactcgcccagaagctaggtgtagagcagcctacattgtattggcatgtaaaaaataagcgggctttgctcg
acgccttagccattgagatgttagataggcaccatactcacttttgccctttagaaggggaaagctggcaagattttttacgtaat
aacgctaaaagttttagatgtgctttactaagtcatcgcgatggagcaaaagtacatttaggtacacggcctacagaaaaacagta
tgaaactctcgaaaatcaattagcctttttatgccaacaaggtttttcactagagaatgcattatatgcactcagcgctgtggggc
attttactttaggttgcgtattggaagatcaagagcatcaagtcgctaaagaagaaagggaaacacctactactgatagtatgccg
ccattattacgacaagctatcgaattatttgatcaccaaggtgcagagccagccttcttattcggccttgaattgatcatatgcgg
attagaaaaacaacttaaatgtgaaagtgggtccgctgcaaacgacgaaaactacgctttagtagcttaataa
})

BBa_C0012_seq : DNASequence(
{
atggtgaatgtgaaaccagtaacgttatacgatgtcgcagagtatgccggtgtctcttatcagaccgtttccc
gcgtggtgaaccaggccagccacgtttctgcgaaaacgcgggaaaaagtggaagcggcgatggcggagctgaattacattcccaaccg
cgtggcacaacaactggcgggcaaacagtcgttgctgattggcgttgccacctccagtctggccctgcacgcgccgtcgcaaattgtc
gcggcgattaaatctcgcgccgatcaactgggtgccagcgtggtggtgtcgatggtagaacgaagcggcgtcgaagcctgtaaagcgg
cggtgcacaatcttctcgcgcaacgcgtcagtgggctgatcattaactatccgctggatgaccaggatgccattgctgtggaagctgc
ctgcactaatgttccggcgttatttcttgatgtctctgaccagacacccatcaacagtattattttctcccatgaagacggtacgcga
ctgggcgtggagcatctggtcgcattgggtcaccagcaaatcgcgctgttagcgggcccattaagttctgtctcggcgcgtctgcgtc
tggctggctggcataaatatctcactcgcaatcaaattcagccgatagcggaacgggaaggcgactggagtgccatgtccggttttca
acaaaccatgcaaatgctgaatgagggcatcgttcccactgcgatgctggttgccaacgatcagatggcgctgggcgcaatgcgcgcc
attaccgagtccgggctgcgcgttggtgcggatatctcggtagtgggatacgacgataccgaagacagctcatgttatatcccgccgt
taaccaccatcaaacaggattttcgcctgctggggcaaaccagcgtggaccgcttgctgcaactctctcagggccaggcggtgaaggg
caatcagctgttgcccgtctcactggtgaaaagaaaaaccaccctggcgcccaatacgcaaaccgcctctccccgcgcgttggccgat
tcattaatgcagctggcacgacaggtttcccgactggaaagcgggcaggctgcaaacgacgaaaactacgctttagtagcttaataa
})

ECK120029600_seq : DNASequence(
{
ttcagccaaaaaacttaagaccgccggtcttgtccactaccttgcagtaatgcggtggacaggatcggcggttttcttttctcttctcaa
})

#########################################################################

############ Define the inidividual Component Definitions ###############

BBa_J61130_RBS : RBS
    description = "RBS2"
    name = "BBa_J61101 RBS"
    sequence = BBa_J61130_seq

BBa_C0012_CDS : CDS
    description = "lacI coding sequence"
    name = "lacI"
    sequence = BBa_C0012_seq

ECK120033736_terminator : Terminator
    description = "Terminator2" #Not the film..
    name = "ECK120033736"
    sequence = ECK120033736_seq

BBa_R0040_promotor : Promoter
    description = "pTet promoter"
    name = "pTetR"
    sequence = BBa_R0040_seq

Q6QR72_protein : Protein
    description = "TetR protein"
    name = "TetR"

P03023_protein : Protein
    name = "LacI"
    description = "LacI protein"

BBa_J61120_RBS : RBS
    description = "RBS2"
    name = "BBa_J61101 RBS"
    sequence = BBa_J61120_seq

BBa_E0040_CDS : CDS
    description = "gfp coding sequence"
    name = "gfp"
    sequence = BBa_E0040_seq

ECK120029600_terminator : Terminator
    description = "Terminator1"
    name = "ECK120029600"
    sequence = ECK120029600_seq

BBa_J61101_RBS : RBS
    description = "RBS1"
    name = "BBa_J61101 RBS"
    sequence = BBa_J61101_seq

BBa_R0010_promotor : Promoter
    description = "pLacI promoter"
    name = "pLacI"
    sequence = BBa_R0010_seq

P42212_protein : Protein
    description = "GFP protein"
    name = "GFP"

BBa_C0040_CDS : CDS
    description = "tetR coding sequence"
    name = "tetR"
    sequence = BBa_C0040_sequence

############################################################################

################## Define Parent Component Definitions #####################

pIKELeftCassette_1 : DNAComponent
    role = <SBOL:Inverter>
    name="TetR Inverter"
    description = "TetR Inverter"
    component   #By not extending from a access template, the ECK120029600_terminator component defaults to having a public access.
        ECK120029600_terminator
            at InlinePosition(1198..1288)
    component : public  #You can also explicity extend from the public access template which give the value "public" to the access property.
        BBa_R0040_promotor
            at InlinePosition(1..55)
    component : public
        BBa_C0012_CDS
            at InlinePosition(69..1197)
    component : public
        BBa_J61101_RBS
            at InlinePosition(56..68)

pIKERightCassette_1 : DNAComponent
    role = <SBOL:Inverter>
    name="LacI Inverter"
    description = "LacI Inverter"
    component : public
        BBa_R0010_promotor
            at InlinePosition(1..55)
    component : public
        BBa_C0040_CDS
            at InlinePosition(69..729)
    component : public
        BBa_J61130_RBS
            at InlinePosition(730..742)
    component : public
        BBa_E0040_CDS
            at InlinePosition(743..1463)
    component : public
        ECK120033736_terminator
            at InlinePosition(1464..1554)
    component : public
        BBa_J61120_RBS
            at InlinePosition(56..68)

pIKE_Toggle_1 : DNAComponent
    role = <SBOL:Toggle>
    name="LacI/TetR Toggle Switch"
    description = "LacI/TetR Toggle Switch"
    component : public
        pIKELeftCassette_1
            at InlinePosition(1..1285)
    component : public
        pIKERightCassette_1
            at InlinePosition(1286..2834)

############## Define the individual Module Definition #####################

laci_inverter : InverterModule
    functionalComponent : public_io  #populates properties from the public_io property constructor
        P03023_protein as TF

    functionalComponent #By not extending from a access/direction template, the access defaults to "public" and the direction defaults to "inout"
        BBa_R0010_promotor as promoter

    TF inhibits promoter  #Interaction defined in import.

tetr_inverter : InverterModule
    functionalComponent : public_io
        Q6QR72_protein as TF

    functionalComponent : public_io
        BBa_R0040_promotor as promoter

    TF inhibits promoter  #Interaction defined in import.

###########################################################################

##################### Define the model Definition #########################

toggleswitch : ContinuousSBMLModel
    source = <http://virtualparts.org/part/pIKE_Toggle_1>

##########################################################################

############# Define the top level Module Definition ######################

toggle_switch : ToggleModule
    functionalComponent : public_io
        P03023_protein as LacI  #Replacing the displayIds of the functionalcomponents.
    functionalComponent : public_io
        Q6QR72_protein as TetR

    module
        laci_inverter
            LacI implements TF  # implements is an inline MapsTo template
    module
        tetr_inverter
            TetR implements TF

    model
        toggleswitch

###########################################################################
"""



class SequenceConstraints(Templates):

    keys = OrderedDict()

    def __init__(self):
        pass






