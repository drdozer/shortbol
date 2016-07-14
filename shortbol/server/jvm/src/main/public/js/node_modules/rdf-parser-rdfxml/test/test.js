/* global describe, it */
var assert = require('assert')
var rdf = require('rdf-ext')
var testData = require('rdf-test-data')
var testUtils = require('rdf-test-utils')
var RdfXmlParser = require('../')

var simpleXml = '<?xml version="1.0" encoding="UTF-8"?>' +
    '<rdf:RDF xmlns:e="http://example.org/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' +
    '<rdf:Description rdf:about="subject">' +
    '<e:predicate>object</e:predicate>' +
    '</rdf:Description>' +
    '</rdf:RDF>';

describe('RDF/XML parser', function () {
  describe('instance API', function () {
    describe('process', function () {
      it('should be supported', function (done) {
        var parser = new RdfXmlParser()
        var counter = 0

        parser.process(simpleXml, function () {
          counter++
        }).then(function () {
          if (counter !== 1) {
            done('no triple processed')
          } else {
            done()
          }
        }).catch(function (error) {
          done(error)
        })
      })

      it('should use base parameter', function (done) {
        var parser = new RdfXmlParser()
        var counter = 0

        parser.process(simpleXml, function (triple) {
          if (triple.subject.equals('http://example.org/subject')) {
            counter++
          }
        }, 'http://example.org/').then(function () {
          if (counter !== 1) {
            done('no triple processed')
          } else {
            done()
          }
        }).catch(function (error) {
          done(error)
        })
      })

      it('should use filter parameter', function (done) {
        var parser = new RdfXmlParser()
        var processed = false
        var filtered = false

        parser.process(simpleXml, function () {
          processed = true
        }, null, function () {
          filtered = true

          return false
        }).then(function () {
          if (processed || !filtered) {
            done('no triple processed')
          } else {
            done()
          }
        }).catch(function (error) {
          done(error)
        })
      })

      it('should use done parameter', function (done) {
        var parser = new RdfXmlParser()
        var counter = 0

        Promise.resolve(new Promise(function (resolve) {
          parser.process(simpleXml, function () {
            counter++
          }, null, null, function () {
            resolve()
          })
        })).then(function () {
          if (counter !== 1) {
            done('no triple processed')
          } else {
            done()
          }
        }).catch(function (error) {
          done(error)
        })
      })
    })

    describe('callback', function () {
      it('should be supported', function (done) {
        var parser = new RdfXmlParser()

        Promise.resolve(new Promise(function (resolve) {
          parser.parse(simpleXml, function () {
            resolve()
          })
        })).then(function () {
          done()
        }).catch(function (error) {
          done(error)
        })
      })

      it('should forward errors', function (done) {
        var parser = new RdfXmlParser()

        Promise.resolve(new Promise(function (resolve, reject) {
          parser.parse('', function (error) {
            if (error) {
              reject(error)
            } else {
              resolve()
            }
          })
        })).then(function () {
          done('no error thrown')
        }).catch(function () {
          done()
        })
      })
    })

    describe('Promise', function () {
      it('should be supported', function (done) {
        var parser = new RdfXmlParser()

        parser.parse(simpleXml).then(function () {
          done()
        }).catch(function (error) {
          done(error)
        })
      })

      it('should forward error to Promise API', function (done) {
        var parser = new RdfXmlParser()

        parser.parse('').then(function () {
          done('no error thrown')
        }).catch(function () {
          done()
        })
      })
    })

    describe('Stream', function () {
      it('should be supported', function (done) {
        var parser = new RdfXmlParser()
        var counter = 0

        parser.stream(simpleXml).on('data', function () {
          counter++
        }).on('end', function () {
          if (counter !== 1) {
            done('no triple streamed')
          } else {
            done()
          }
        }).on('error', function (error) {
          done(error)
        })
      })
    })
  })

  describe('static API', function () {
    describe('process', function () {
      it('should be supported', function (done) {
        var counter = 0

        RdfXmlParser.process(simpleXml, function () {
          counter++
        }).then(function () {
          if (counter !== 1) {
            done('no triple processed')
          } else {
            done()
          }
        }).catch(function (error) {
          done(error)
        })
      })

      it('should use base parameter', function (done) {
        var counter = 0

        RdfXmlParser.process(simpleXml, function (triple) {
          if (triple.subject.equals('http://example.org/subject')) {
            counter++
          }
        }, 'http://example.org/').then(function () {
          if (counter !== 1) {
            done('no triple processed')
          } else {
            done()
          }
        }).catch(function (error) {
          done(error)
        })
      })

      it('should use filter parameter', function (done) {
        var processed = false
        var filtered = false

        RdfXmlParser.process(simpleXml, function () {
          processed = true
        }, null, function () {
          filtered = true

          return false
        }).then(function () {
          if (processed || !filtered) {
            done('no triple processed')
          } else {
            done()
          }
        }).catch(function (error) {
          done(error)
        })
      })

      it('should use done parameter', function (done) {
        var counter = 0

        Promise.resolve(new Promise(function (resolve) {
          RdfXmlParser.process(simpleXml, function () {
            counter++
          }, null, null, function () {
            resolve()
          })
        })).then(function () {
          if (counter !== 1) {
            done('no triple processed')
          } else {
            done()
          }
        }).catch(function (error) {
          done(error)
        })
      })
    })

    describe('callback', function () {
      it('should be supported', function (done) {
        Promise.resolve(new Promise(function (resolve) {
          RdfXmlParser.parse(simpleXml, function () {
            resolve()
          })
        })).then(function () {
          done()
        }).catch(function (error) {
          done(error)
        })
      })

      it('should forward errors', function (done) {
        Promise.resolve(new Promise(function (resolve, reject) {
          RdfXmlParser.parse('', function (error) {
            if (error) {
              reject(error)
            } else {
              resolve()
            }
          })
        })).then(function () {
          done('no error thrown')
        }).catch(function () {
          done()
        })
      })
    })

    describe('Promise', function () {
      it('should be supported', function (done) {
        RdfXmlParser.parse(simpleXml).then(function () {
          done()
        }).catch(function (error) {
          done(error)
        })
      })

      it('should forward error to Promise API', function (done) {
        RdfXmlParser.parse('').then(function () {
          done('no error thrown')
        }).catch(function () {
          done()
        })
      })
    })

    describe('Stream', function () {
      it('should be supported', function (done) {
        var counter = 0

        RdfXmlParser.stream(simpleXml).on('data', function () {
          counter++
        }).on('end', function () {
          if (counter !== 1) {
            done('no triple streamed')
          } else {
            done()
          }
        }).on('error', function (error) {
          done(error)
        })
      })
    })
  })

  describe('example data', function () {
    it('card.xml should be parsed', function (done) {
      var parser = new RdfXmlParser()

      testUtils.readFile('support/card.xml', __dirname).then(function (card) {
        return parser.parse(card, null, 'https://www.example.com/john/card')
      }).then(function (graph) {
        assert(testData.cardGraph.equals(graph))

        done()
      }).catch(function (error) {
        done(error)
      })
    })

    it('install.rdf should be parsed', function (done) {
      var parser = new RdfXmlParser()

      testUtils.readFile('support/install.rdf', __dirname).then(function (rdf) {
        return parser.parse(rdf);
      }).then(function () {
        done()
      }).catch(function (error) {
        done(error)
      })
    })

    /* it('card.json should feed prefix map', function (done) {
      var parser = new RdfXmlParser()

      if (rdf.prefixes.cert) {
        delete rdf.prefixes.cert
      }

      if (rdf.prefixes.foaf) {
        delete rdf.prefixes.foaf
      }

      testUtils.readFile('support/card.json', __dirname).then(function (card) {
        return parser.parse(card, null, 'https://www.example.com/john/card')
      }).then(function () {
        assert.equal(rdf.prefixes.cert, 'http://www.w3.org/ns/auth/cert#')
        assert.equal(rdf.prefixes.foaf, 'http://xmlns.com/foaf/0.1/')
      }).then(function () {
        done()
      }).catch(function (error) {
        done(error)
      })
    }) */
  })
})
