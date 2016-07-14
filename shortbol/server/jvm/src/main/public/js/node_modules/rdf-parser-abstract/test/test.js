/* global describe, it */
var rdf = require('rdf-ext')
var util = require('util')
var AbstractParser = require('../')

function DummyParser () {
  AbstractParser.call(this, rdf)
}

util.inherits(DummyParser, AbstractParser)

DummyParser.prototype.process = function (dataArray, callback, base, filter, done) {
  dataArray.forEach(function (data) {
    if (filter && !filter(data)) {
      return
    }

    callback(data)
  })

  done()
}

var simpleTriple = rdf.createTriple(
  rdf.createNamedNode('http://example.org/subject'),
  rdf.createNamedNode('http://example.org/predicate'),
  rdf.createLiteral('object'))

describe('rdf-parser-abstract', function () {
  it('should not swallow error in callback', function (done) {
    var parser = new DummyParser()

    var mochaExceptionHandler = process.listeners('uncaughtException').pop()
    process.removeListener('uncaughtException', mochaExceptionHandler)

    process.once('uncaughtException', function () {
      process.listeners('uncaughtException').push(mochaExceptionHandler)

      done()
    })

    parser.parse([simpleTriple], function (error, graph) {
      if (error) {}

      throw new Error('test')
    })
  })
})
