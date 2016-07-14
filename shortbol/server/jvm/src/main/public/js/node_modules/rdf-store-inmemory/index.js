var util = require('util')
var AbstractStore = require('rdf-store-abstract')

function iriToKey (iri) {
  // default graph
  if (iri === true) {
    return iri
  }

  // all graphs
  if (!iri) {
    return null
  }

  return iri.toString()
}

function InMemoryStore (options) {
  options = options || {}

  this.rdf = options.rdf || require('rdf-ext')
  this.graphs = {}

  AbstractStore.call(this)
}

util.inherits(InMemoryStore, AbstractStore)

InMemoryStore.prototype.add = function (iri, graph, callback) {
  var self = this

  iri = iriToKey(iri)
  callback = callback || function () {}

  return new Promise(function (resolve) {
    self.graphs[iri] = self.rdf.createGraph()
    self.graphs[iri].addAll(graph)

    callback(null, graph)
    resolve(graph)
  })
}

InMemoryStore.prototype.delete = function (iri, callback) {
  var self = this

  iri = iriToKey(iri)
  callback = callback || function () {}

  return new Promise(function (resolve) {
    if (iri in self.graphs) {
      delete self.graphs[iri]
    }

    callback()
    resolve()
  })
}

InMemoryStore.prototype.graph = function (iri, callback) {
  var self = this

  iri = iriToKey(iri)
  callback = callback || function () {}

  return new Promise(function (resolve) {
    var graph = null

    if (iri) {
      graph = self.graphs[iri]
    } else {
      graph = self.rdf.createGraph()

      Object.keys(self.graphs).forEach(function (iri) {
        graph.addAll(self.graphs[iri])
      })
    }

    callback(null, graph)
    resolve(graph)
  })
}

module.exports = InMemoryStore
