var jsonldNormalize = require('./jsonld-normalize')

function createPlainNode (node) {
  if (!node) {
    return null
  }

  var plain = {}

  if (node.interfaceName === 'NamedNode') {
    plain.type = 'IRI'
    plain.value = node.nominalValue
  } else if (node.interfaceName === 'BlankNode') {
    plain.type = 'blank node'
    plain.value = node.toString()
  } else if (node.interfaceName === 'Literal') {
    plain.type = 'literal'
    plain.value = node.nominalValue
    plain.datatype = node.datatype.nominalValue
    plain.language = node.language
  }

  return plain
}

function createPlainTriple (triple) {
  return {
    subject: createPlainNode(triple.subject),
    predicate: createPlainNode(triple.predicate),
    object: createPlainNode(triple.object)
  }
}

function createPlainDataset (graph) {
  var dataset = {}

  graph.forEach(function (quad) {
    var name = '@default'

    if ('graph' in quad && quad.graph.nominalValue) {
      name = quad.graph.nominalValue
    }

    if (!(name in dataset)) {
      dataset[name] = []
    }

    dataset[name].push(createPlainTriple(quad))
  })

  return dataset
}

function normalize (graph) {
  return jsonldNormalize(createPlainDataset(graph))
}

module.exports = normalize
