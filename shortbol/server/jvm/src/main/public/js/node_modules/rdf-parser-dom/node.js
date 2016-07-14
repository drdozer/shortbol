var util = require('util')
var xmldom = require('xmldom')
var AbstractParser = require('rdf-parser-abstract')

function DomParser (rdf) {
  AbstractParser.call(this, rdf)
}

util.inherits(DomParser, AbstractParser)

DomParser.prototype.parseHtmlDom = function (toparse, base) {
  var parser = new (xmldom.DOMParser)()

  parser.options.errorHandler = {
    warning: function () {},
    error: function () {},
    fatalError: function () {}
  }

  return parser.parseFromString(toparse, 'text/html')
}

DomParser.prototype.parseXmlDom = function (toparse, base) {
  var parser = new (xmldom.DOMParser)()

  parser.options.errorHandler = {
    warning: function () {},
    error: function () {},
    fatalError: function () {}
  }

  return parser.parseFromString(toparse, 'application/xml')
}

module.exports = DomParser
