# rdf-store-inmemory

[![Build Status](https://travis-ci.org/rdf-ext/rdf-store-inmemory.svg?branch=master)](https://travis-ci.org/rdf-ext/rdf-store-inmemory)
[![NPM Version](https://img.shields.io/npm/v/rdf-store-inmemory.svg?style=flat)](https://npm.im/rdf-store-inmemory)

In Memory RDF Store that follows the [RDF Interface](http://bergos.github.io/rdf-ext-spec/) specification

## Install

```
npm install --save rdf-store-inmemory
```

## Usage

A simple in-memory triple store implementation. Cross graph read operations are supported by using `undefined` as graph IRI. In that case `.graph` returns all graphs merged into a single graph and `.match` operates on that single merged graph. Because there is nothing to configure, the constructor doesn't require any parameters. (Originally from [here](https://github.com/zazukoians/rdf-ext#inmemorystore))

## History

Taken from [zazukoians/rdf-ext](https://github.com/zazukoians/rdf-ext)

## Licence

MIT