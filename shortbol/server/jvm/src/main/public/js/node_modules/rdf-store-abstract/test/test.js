/* global before, describe, it */
var assert = require('assert')
var rdf = require('rdf-ext')
var AbstractStore = require('../')

describe('rdf-store-abstract', function () {
  var graph1 = rdf.createGraph()
  var graph2 = rdf.createGraph()

  before(function () {
    graph1.add(rdf.createTriple(
      rdf.createNamedNode('http://example.org/subject1'),
      rdf.createNamedNode('http://example.org/predicate'),
      rdf.createLiteral('object1')))

    graph1.add(rdf.createTriple(
      rdf.createNamedNode('http://example.org/subject1'),
      rdf.createNamedNode('http://example.org/predicate'),
      rdf.createLiteral('object2')))

    graph2.add(rdf.createTriple(
      rdf.createNamedNode('http://example.org/subject2'),
      rdf.createNamedNode('http://example.org/predicate'),
      rdf.createLiteral('object3')))
  })

  it('.add should throw an error', function () {
    var store = new AbstractStore()
    var error

    try {
      store.add('http://example.org/graph', rdf.createGraph())
    } catch (err) {
      error = err
    }

    assert.notEqual(error, null)
  })

  it('.delete should throw an error', function () {
    var store = new AbstractStore()
    var error

    try {
      store.delete('http://example.org/graph')
    } catch (err) {
      error = err
    }

    assert.notEqual(error, null)
  })

  it('.graph should throw an error', function () {
    var store = new AbstractStore()
    var error

    try {
      store.graph('http://example.org/graph')
    } catch (err) {
      error = err
    }

    assert.notEqual(error, null)
  })

  it('.match should return a graph that contains all triples matching the given pattern', function (done) {
    var store = new AbstractStore()

    store.graph = function (iri) {
      if (iri === 'http://example.org/graph1') {
        return Promise.resolve(graph1)
      } else {
        return Promise.resolve(graph1.merge(graph2))
      }
    }

    store.match('http://example.org/subject1', null, null, 'http://example.org/graph1').then(function (graph) {
      assert.equal(graph.length, 2)
    }).then(function () {
      return store.match(null, 'http://example.org/predicate', null, 'http://example.org/graph1')
    }).then(function (graph) {
      assert.equal(graph.length, 2)
    }).then(function () {
      return store.match(null, null, 'object1', 'http://example.org/graph1')
    }).then(function (graph) {
      assert.equal(graph.length, 1)
    }).then(function () {
      return store.match(null, null, 'object3', 'http://example.org/graph1')
    }).then(function (graph) {
      assert.equal(graph.length, 0)

      done()
    }).catch(function (error) {
      done(error)
    })
  })

  it('.merge should add a graph if the store does not contain the named graph', function (done) {
    var store = new AbstractStore()

    store.graph = function (iri) {
      return Promise.resolve()
    }

    store.add = function (iri, graph) {
      return Promise.resolve(graph)
    }

    store.merge('http://example.org/graph', graph1).then(function (graph) {
      assert.equal(graph.equals(graph1), true)

      done()
    }).catch(function (error) {
      done(error)
    })
  })

  it('.merge should merge a graph with the one the store contains', function (done) {
    var store = new AbstractStore()
    var mergedGraph = graph2.merge(rdf.createGraph())

    store.graph = function (iri) {
      return Promise.resolve(mergedGraph)
    }

    store.add = function (iri, graph) {
      mergedGraph = graph

      return Promise.resolve(graph)
    }

    store.merge('http://example.org/graph', graph1).then(function (graph) {
      assert.equal(graph.equals(graph1), true)
      assert.equal(mergedGraph.equals(graph1.merge(graph2)), true)

      done()
    }).catch(function (error) {
      done(error)
    })
  })

  it('.remove should remove all triple given in the graph', function (done) {
    var store = new AbstractStore()
    var trimmedGraph = graph1.merge(graph2)

    store.graph = function (iri) {
      return Promise.resolve(trimmedGraph)
    }

    store.add = function (iri, graph) {
      trimmedGraph = graph

      return Promise.resolve(graph)
    }

    store.remove('http://example.org/graph', graph1).then(function () {
      assert.equal(trimmedGraph.equals(graph2), true)

      done()
    }).catch(function (error) {
      done(error)
    })
  })

  it('.removeMatches should remove triples based on the given pattern', function (done) {
    var store = new AbstractStore()
    var localGraph1 = graph1
    var localGraph2 = graph2

    store.graph = function (iri) {
      if (iri === 'http://example.org/graph1') {
        return Promise.resolve(graph1.clone())
      } else {
        return Promise.resolve(graph2.clone())
      }
    }

    store.add = function (iri, graph) {
      if (iri === 'http://example.org/graph1') {
        return Promise.resolve(localGraph1 = graph)
      } else {
        return Promise.resolve(localGraph2 = graph)
      }
    }

    store.removeMatches('http://example.org/subject1', null, null, 'http://example.org/graph1').then(function (graph) {
      assert.equal(localGraph1.length, 0)
      assert.equal(localGraph2.length, 1)

      localGraph1 = graph1
    }).then(function () {
      return store.removeMatches(null, 'http://example.org/predicate', null, 'http://example.org/graph1')
    }).then(function (graph) {
      assert.equal(localGraph1.length, 0)
      assert.equal(localGraph2.length, 1)

      localGraph1 = graph1
    }).then(function () {
      return store.removeMatches(null, null, 'object1', 'http://example.org/graph1')
    }).then(function (graph) {
      assert.equal(localGraph1.length, 1)
      assert.equal(localGraph2.length, 1)

      localGraph1 = graph1
    }).then(function () {
      return store.removeMatches(null, null, 'object3', 'http://example.org/graph1')
    }).then(function (graph) {
      assert.equal(localGraph1.length, 2)
      assert.equal(localGraph2.length, 1)

      done()
    }).catch(function (error) {
      done(error)
    })
  })
})
