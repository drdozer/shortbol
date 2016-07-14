# rdf-parser-abstract

[![Build Status](https://travis-ci.org/rdf-ext/rdf-parser-abstract.svg?branch=master)](https://travis-ci.org/rdf-ext/rdf-parser-abstract)
[![NPM Version](https://img.shields.io/npm/v/rdf-parser-abstract.svg?style=flat)](https://npm.im/rdf-parser-abstract)

Abstract base class for RDF-Interfaces parser implementations.

## Usage

The `AbstractParser` class adds default implementations for `.parse(data, callback, base, filter, graph)` and `.stream(inputStream, base, filter)`.
Only the `.process(data, callback, base, filter, done)` method must be implemented.

Your own parser hast to inherit from the abstract parser.
First let's install the package.
Start a terminal and run:

	npm install rdf-abstract-parser --save

Now let's start coding.
To inherit from the abstract parser, we have to load the util and the `AbstractParser` itself first:

	var util = require('util')
	var AbstractParser = require('rdf-abstract-parser')

Your parser must accept the optional parameter `rdf` to use the given RDF environment.
To initialize the `AbstractParser`, you have to call the `AbstractParser` constructor in your own constructor.

	function YourParser (options) {
	  // use the given RDF environment or RDF-Ext if none was given
	  this.rdf = options.rdf || require('rdf-ext')

    // call the AbstractParser constructor
	  AbstractParser.call(this, rdf)
	}

After the constructor code you have to inherit with the `util.inherits` function.

	util.inherits(YourParser, AbstractParser)

Only the `.process` method is required to be implemented.
The `base` parameter is optional, let's check if it is given and createt a `NamedNode` from the string, so we can use it for the `graph` property for the quads.
The `filter` parameter is optional, so let's assign a accept all triples filter, if none was given.
Finally you can implement your parser code.
The example code accepts strings with one triple per line and spo space separated and only named node without additional escaping.
To parse the string we only have to split the string per line and than split every line into spo.
For every spo we create a quad with the RDF environment and check if the filter accepts the quad.
If the filter accepts the quad we can forward it.
That's it, your parser is complete!

	YourParser.prototype.process = function (data, callback, base, filter, done) {
	  var self = this

    // create a NamedNode if the base parameter is given
    base = !!base ? null : self.rdf.createNamedNode(base.toString())

	  // use a accept all triples filter, if none was given
	  filter = filter || function () {
	    return true
	  }

	  // split string into triples
	  data.split('\n').forEach(function (line) {
	    // split triple into spo
	    var spo = line.split(' ')

	    // create the Quad using the RDF environment
	    var quad = self.rdf.createQuad(
	      self.rdf.createNamedNode(spo[0]),
	      self.rdf.createNamedNode(spo[1]),
	      self.rdf.createNamedNode(spo[2]),
	      base)

      // only forward the quad if the filter accepts it
	    if (filter(quad)) {
	      // forward the quad
      	callback(quad)
      }
	  })
	}
