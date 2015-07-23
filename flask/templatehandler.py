__author__ = 'chris'

from templates import *

class TemplateHandler():


    def __init__(self):

        self.moduleDefinitions = ModuleDefinitions()
        self.propertyConstructors = PropertyConstructors()
        self.sequences = Sequences()
        self.interactions = Interactions()
        self.participations = Participations()
        self.sequenceAnnotations = SequenceAnnotations()
        self.componentDefinitions = ComponentDefinitions()
        self.mapstos = MapsTos()
        self.models = Models()