var normalize = require('rdf-normalize')

var rdf = {}

rdf.encodeString = function (s) {
  var out = ''
  var skip = false
  var _g1 = 0
  var _g = s.length

  while (_g1 < _g) {
    var i = _g1++

    if (!skip) {
      var code = s.charCodeAt(i)

      if (code >= 55296 && code <= 56319) {
        var low = s.charCodeAt(i + 1)
        code = (code - 55296) * 1024 + (low - 56320) + 65536
        skip = true
      }

      if (code > 1114111) {
        throw new Error('Char out of range')
      }

      var hex = '00000000'.concat((Number(code)).toString(16).toUpperCase())

      if (code >= 65536) {
        out += '\\' + hex.slice(-8)
      } else {
        if (code >= 127 || code <= 31) {
          switch (code) {
            case 9: out += '\\t'; break
            case 10: out += '\\n'; break
            case 13: out += '\\r'; break
            default: out += '\\u' + hex.slice(-4); break
          }
        } else {
          switch (code) {
            case 34: out += '\\"'; break
            case 92: out += '\\\\'; break
            default: out += s.charAt(i); break
          }
        }
      }
    } else {
      skip = !skip
    }
  }
  return out
}

rdf.AbstractGraph = function (constructor, other) {
  this.constructor = constructor

  Object.defineProperty(this, 'length', {
    get: function () { return this.toArray().length }
  })

  if (other) {
    this.addAll(other)
  }
}

rdf.AbstractGraph.prototype.addAll = function (other) {
  var self = this

  other.forEach(function (quad) {
    self.add(quad)
  })

  return this
}

rdf.AbstractGraph.prototype.clone = function () {
  return new this.constructor(this)
}

rdf.AbstractGraph.prototype.difference = function (other) {
  var difference = new this.constructor()

  this.forEach(function (quad) {
    if (!other.includes(quad)) {
      difference.add(quad)
    }
  })

  return difference
}

rdf.AbstractGraph.prototype.equals = function (other) {
  return normalize(this) === normalize(other)
}

rdf.AbstractGraph.prototype.every = function (callback) {
  return this.toArray().every(callback)
}

rdf.AbstractGraph.prototype.filter = function (callback) {
  return new this.constructor(this.toArray().filter(callback))
}

rdf.AbstractGraph.prototype.forEach = function (callback) {
  var self = this

  self.toArray().forEach(function (quad) {
    callback(quad, self)
  })
}

rdf.AbstractGraph.prototype.includes = function (quad) {
  return this.match(quad.subject, quad.predicate, quad.object, quad.graph).length === 1
}

rdf.AbstractGraph.prototype.intersection = function (other) {
  var intersection = new this.constructor()

  this.forEach(function (quad) {
    if (other.includes(quad)) {
      intersection.add(quad)
    }
  })

  return intersection
}

rdf.AbstractGraph.prototype.map = function (callback) {
  var self = this

  return self.toArray().map(function (quad) {
    return callback(quad, self)
  })
}

rdf.AbstractGraph.prototype.match = function (subject, predicate, object, graph) {
  return new rdf.Graph(this.toArray().filter(function (quad) {
    if (graph && (!quad.graph || !quad.graph.equals(graph))) {
      return false
    }

    if (subject && !quad.subject.equals(subject)) {
      return false
    }

    if (predicate && !quad.predicate.equals(predicate)) {
      return false
    }

    if (object && !quad.object.equals(object)) {
      return false
    }

    return true
  }))
}

rdf.AbstractGraph.prototype.merge = function (other) {
  return this.clone().addAll(other)
}

rdf.AbstractGraph.prototype.removeMatches = function (subject, predicate, object, graph) {
  var self = this
  var matches = self.match(subject, predicate, object, graph)

  matches.forEach(function (quad) {
    self.remove(quad)
  })

  return self
}

rdf.AbstractGraph.prototype.some = function (callback) {
  return this.toArray().some(callback)
}

rdf.AbstractGraph.prototype.toString = function () {
  return this.toArray()
    .map(function (quad) {
      return quad.toString()
    })
    .join('\n')
}

module.exports = rdf
