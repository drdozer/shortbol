/* global before, describe, it */
var _ = require('lodash')
var assert = require('assert')
var normalize = require('../index')
var testUtils = require('rdf-test-utils')
var N3Parser = require('rdf-parser-n3')

var ns = 'http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#'
var entries = ns + 'entries'

function listItems (graph, entry) {
  var items = []

  while(!entry.equals('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil')) {
    items.push(graph.match(entry, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#first').toArray().shift().object)
    entry = graph.match(entry, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest').toArray().shift().object
  }

  return items
}

var exclude = [
  'manifest-urgna2012#test044',
  'manifest-urgna2012#test045',
  'manifest-urgna2012#test046',
  'manifest-urdna2015#test023',
  'manifest-urdna2015#test044',
  'manifest-urdna2015#test045',
  'manifest-urdna2015#test046',
  'manifest-urdna2015#test057'
]

function processTest (graph, subject) {
  if (exclude.indexOf(subject.nominalValue) !== -1) {
    return function () {}
  }

  return function () {
    it('should pass test ' + subject.toString(), function (done) {
      var action = graph
        .match(subject, 'http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#action')
        .toArray()
        .shift()
        .object
        .nominalValue
      var result = graph
        .match(subject, 'http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#result')
        .toArray()
        .shift()
        .object
        .nominalValue

      Promise.all([
        testUtils.readFile('./support/' + action, __dirname),
        testUtils.readFile('./support/' + result, __dirname)
      ]).then(function (contents) {
        var input = contents[0]
        var expected = contents[1]

        return N3Parser.parse(input).then(function (graph) {
          var actual = normalize(graph)

          assert.equal(actual, expected)

          done()
        })
      }).catch(function (error) {
        done(error)
      })
    })
  }
}

function processManifest (filename) {
  return testUtils.readFile(filename, __dirname).then(function (content) {
    return N3Parser.parse(content)
  }).then(function (graph) {
    var testEntry = graph.match(null, entries).toArray().shift().object

    return listItems(graph, testEntry).map(function (testSubject) {
      return processTest(graph, testSubject)
    })
  })
}

Promise.all([
  processManifest('./support/manifest-urgna2012.ttl'),
  processManifest('./support/manifest-urdna2015.ttl')
]).then(function(tests) {
  var tests2012 = tests[0]
  var tests2015 = tests[1]

  describe('normalize', function () {
    describe('RDF Graph Normalization (URGNA2012)', function () {
      tests2012.forEach(function (test) {
        test()
      })
    })

    describe('RDF Dataset Normalization (URDNA2015)', function () {
      tests2015.forEach(function (test) {
        test()
      })
    })
  })

  run()
}).catch(function (error) {
  console.error(error.stack || error.message)
})
