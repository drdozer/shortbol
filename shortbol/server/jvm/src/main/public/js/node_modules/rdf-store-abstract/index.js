function AbstractStore () {
}

AbstractStore.prototype.add = function () {
  throw new Error('not implemented')
}

AbstractStore.prototype.delete = function () {
  throw new Error('not implemented')
}

AbstractStore.prototype.graph = function () {
  throw new Error('not implemented')
}

AbstractStore.prototype.match = function (subject, predicate, object, iri, callback, limit) {
  var self = this

  callback = callback || function () {}

  return self.graph(iri).then(function (graph) {
    if (graph) {
      graph = graph.match(subject, predicate, object, limit)
    }

    callback(null, graph)

    return graph
  })
}

AbstractStore.prototype.merge = function (iri, graph, callback) {
  var self = this

  callback = callback || function () {}

  return self.graph(iri).then(function (existing) {
    if (existing) {
      existing.addAll(graph)
    } else {
      existing = graph
    }

    return self.add(iri, existing, callback)
  }).then(function () {
    return graph
  })
}

AbstractStore.prototype.remove = function (iri, graph, callback) {
  var self = this

  callback = callback || function () {}

  return self.graph(iri).then(function (existing) {
    if (existing) {
      return self.add(iri, existing.difference(graph))
    }
  }).then(callback)
}

AbstractStore.prototype.removeMatches = function (subject, predicate, object, iri, callback) {
  var self = this

  callback = callback || function () {}

  return self.graph(iri).then(function (existing) {
    if (existing) {
      return self.add(iri, existing.removeMatches(subject, predicate, object))
    }
  }).then(callback)
}

module.exports = AbstractStore
