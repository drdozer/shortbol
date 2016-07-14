var browserify = require('browserify')
var fs = require('fs')
var path = require('path')

browserify('./test/test.js').bundle().pipe(fs.createWriteStream('./test/support/browser-test.js'))

fs.unlinkSync('./test/support/mocha.css')
fs.unlinkSync('./test/support/mocha.js')
fs.symlinkSync(path.join(path.dirname(require.resolve('mocha')), 'mocha.css'), './test/support/mocha.css')
fs.symlinkSync(path.join(path.dirname(require.resolve('mocha')), 'mocha.js'), './test/support/mocha.js')
