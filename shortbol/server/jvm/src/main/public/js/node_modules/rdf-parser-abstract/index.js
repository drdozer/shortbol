var concatStream = require('concat-stream')
var util = require('util')
var Readable = require('stream').Readable

function AbstractParser (rdf) {
  this.rdf = rdf
}

AbstractParser.prototype.parse = function (data, callback, base, filter, graph) {
  var self = this

  graph = graph || self.rdf.createGraph()

  var pushTriple = function (triple) {
    graph.add(triple)
  }

  return new Promise(function (resolve, reject) {
    self.process(data, pushTriple, base, filter, function (error) {
      // callback API
      if (callback) {
        process.nextTick(function () {
          if (error) {
            callback(error)
          } else {
            callback(null, graph)
          }
        })
      }

      // Promise API
      if (error) {
        reject(error)
      } else {
        resolve(graph)
      }
    })
  })
}

AbstractParser.prototype.stream = function (inputStream, base, filter) {
  var self = this

  var outputStream = new AbstractParser.TripleReadStream()

  AbstractParser.streamToData(inputStream).then(function (data) {
    self.process(data, function (triple) {
      outputStream.push(triple)
    }, base, filter, function (error) {
      if (error) {
        outputStream.emit('error', error)
      } else {
        outputStream.emit('end')
      }
    })
  }).catch(function (error) {
    outputStream.emit('error', error)
  })

  return outputStream
}

AbstractParser.streamToData = function (stream) {
  return new Promise(function (resolve, reject) {
    if (typeof stream !== 'object' || typeof stream.read !== 'function') {
      return resolve(stream)
    }

    stream.on('error', function (error) {
      reject(error)
    })

    stream.pipe(concatStream(function (data) {
      resolve(data)
    }))
  })
}

AbstractParser.TripleReadStream = function () {
  Readable.call(this, {objectMode: true})

  this._read = function () {
    return 0
  }
}

util.inherits(AbstractParser.TripleReadStream, Readable)

module.exports = AbstractParser
