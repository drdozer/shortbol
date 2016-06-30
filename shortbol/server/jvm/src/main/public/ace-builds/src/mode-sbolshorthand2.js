/**
 * Created by Chris Taylor on 23/07/15.
 */
var prefixtok = "prefix";
var localnametok = "localName";
var infixconstok = "support.function.InfixConstructor";
var prefixConstok = "support.function.PrefixConstructor";
var qnametok = "constant.buildin.constant.other.qname";
var urltok = "constant.buildin.constant.other.url";
var integertok = "constant.numeric";
var keywordtok = "keyword";
var commentok = "comment";
var stringtok = "string";
var operatortok = "keyword.operator";
var propertytok = "property";
var templatetok = "template";
var texttok = "text";
var lefteclipsetok = "lefteclipse";
var righteclipsetok = "righteclipse";
var paramtok = "params";



define("ace/mode/sbol_highlight_rules", ["require", "exports", "module", "ace/lib/oop", "ace/mode/text_highlight_rules"], function (require, exports, module) {

    var oop = require("../lib/oop");
    var TextHighlightRules = require("./text_highlight_rules").TextHighlightRules;

    var SBOLHighlightRules = function () {

        var strPre = "(?:r|u|ur|R|U|UR|Ur|uR)?";

        var decimalInteger = "(?:(?:[1-9]\\d*)|(?:0))";
        var integer = "(?:" + decimalInteger + ")";

        var keywords = ["as", "at", "import"];
        var keywordsstring = "";
        var keywordsregex = "\\b(";

        var localnameregex = "[a-zA-Z_][a-zA-Z0-9_\\-.]*";
        var inlinetemplateregex = "[a-z_][a-z0-9_.\\-]*";

        var urlregex = "<[a-zA-Z0-9_\\-:/.~!*'();@&=,?%#\\[\\]]+>";
        var qnameregex = "([a-zA-Z0-9_\\-.]+)(:)([a-zA-Z0-9_\\-.]+)";
        var paramregex = "[(\\w+,?)]*";
        var pragmaregex = "(@)(\\w+)";

        for (var i = 0; i < keywords.length; i++) {
            if ((keywords.length - 1) == i) {
                keywordsregex += keywords[i] + ")\\b";
                keywordsstring += keywords[i]
            }
            else {
                keywordsregex += keywords[i] + "|";
                keywordsstring += keywords[i] + "|";
            }
        }

        var keywordMapper = this.createKeywordMapper({
        "keyword": keywordsstring
            }, "identifier");

        var CheckIdentifierType = function(value){
            var qnametest = new RegExp(qnameregex);
            var urltest = new RegExp(urlregex);
            var identifiertest = new RegExp(localnameregex)
            if (qnametest.test(value)) {
                return qnametok
            }
            else if (urltest.test(value)) {
                return urltok
            }
            else if (identifiertest.test(value)){
                return localnametok
            }
            };

        this.$rules = {
            "start": [
                {
                    // url
                    token: urltok,
                    regex:  urlregex

                },

                {
                    // qname
                    token: [prefixtok, operatortok, localnametok],
                    regex:  qnameregex

                },

                {
                    // pragma
                    token: [operatortok, keywordtok],
                    regex: pragmaregex
                },

                {
                    token: operatortok,
                    regex: ":"
                },

                {
                    token: operatortok,
                    regex: "=>"
                },

                {
                    token: function (word1, word2, word3) {
                        word1 = word1.trim();
                        word2 = word2.trim();
                        word3 = word3.trim(); //remove trailing whitespace
                        word1tok = CheckIdentifierType(word1);
                        word3tok = CheckIdentifierType(word3);
                        var word2token = infixconstok;
                        var index = keywords.indexOf(word2);
                        if (index != -1) {
                            word2token = keywordtok
                        }
                        var tokenarray = [word1tok, word2token, word3tok];
                        return tokenarray
                    },
                    regex: "(" + urlregex + "[\\s\t]+|" + localnameregex + "[\\s\t]+)(" + inlinetemplateregex + ")([\\s\t]+" + urlregex + "|[\\s\t]+" + localnameregex +")"

                },

                {
                    token: function(value1,value2, value3,value4) {
                        return [templatetok, paramtok, "keyword.rightarrowstate", "keyword.rightarrowstate"]
                    },
                    regex: "^([\\s\t]*" + localnameregex + ")" + "(\\(" + paramregex + "\\))?" + "([\\s\t]*=>)",        // =>
                    next: "sboltemp"
                },{
                        token: function(value1,value2) {
                        return [propertytok,"keyword.operator"]
                    },

                    regex : "^([\\s\t]*" + localnameregex + ")" + "([\\s\t]*=)"        // assignment start state
                },
                {
                    token: function(value1,value2,value3) {
                        return [localnametok,"keyword.colonstate"]
                    },

                    regex : "^([\\s\t]*" + localnameregex + ")" + "([\\s\t]*:)",         // :
                    next: "sboltemp"
                },

                {
                    token: commentok,
                    regex: /#[^\n]+/

                }, {
                    token: keywordtok,
                    regex: keywordsregex  // /\b(as|at|import)\b/
                },
                {
                    token: keywordtok,
                    regex: /(\.\.)/
                },
                {
                    token: stringtok,           //multi line string.
                    regex: strPre + '{',
                    next: "qqstring3"
                }, {
                    token: stringtok,           // " string
                    regex: strPre + '"(?=.)',
                    next: "qqstring"
                }, {
                    token: stringtok,           // ' string
                    regex: strPre + "'(?=.)",
                    next: "qstring"
                },
                {
                    token: integertok, // integer
                    regex: "\\b" + integer + "\\b"
                }, {
                    token: operatortok,
                    regex: "="
                }, {
                    token: texttok,
                    regex: localnameregex
                },
                {
                    token: lefteclipsetok,
                    regex : "[\\s\t]*\\("
                },
                {
                    token: righteclipsetok,
                    regex: "[\\s\t]*\\)"
                }
            ],
            "sboltemp": [
                {
                 token: function(value1,value2) {
                        return [prefixConstok,"keyword.colonstate"]
                    },

                    regex : "([\\s\t]*" + localnameregex + ")" + "([\\s\t]*:)",         // :
                    next: "sboltemp"

            }, {
                  token: function(value1,value2) {
                        return [prefixConstok,"keyword.colonstate"]
                    },

                    regex : "([\\s\t]*" + localnameregex + ")" + "([\\s\t]*=>)",         // :
                    next: "sboltemp"
            },

                {
                    token: function(val){
                        return prefixConstok
                    },
                    regex: "[\\s\t]*" + localnameregex,//+ ")([\\s\t]*\\(\\w*\\))*",
                    next: "start"

        }, {
                defaultToken: "commastate"
            }],
            "qqstring3": [{
            }, {
                token: stringtok,
                regex: '}',
                next: "start"
            }, {
                defaultToken: stringtok
            }],
            "qqstring": [{
            }, {
                token: stringtok,
                regex: "\\\\$",
                next: "qqstring"
            }, {
                token: stringtok,
                regex: '"|$',
                next: "start"
            }, {
                defaultToken: stringtok
            }],
            "qstring": [{
            }, {
                token: stringtok,
                regex: "\\\\$",
                next: "qstring"
            }, {
                token: stringtok,
                regex: "'|$",
                next: "start"
            }, {
                defaultToken: stringtok
            }]
            
        };

    };

    oop.inherits(SBOLHighlightRules, TextHighlightRules);

    exports.SBOLHighlightRules = SBOLHighlightRules;
});

define("ace/mode/folding/sbolshorthandfold", ["require", "exports", "module", "ace/lib/oop", "ace/mode/folding/fold_mode"], function (require, exports, module) {
    "use strict";

    var oop = require("../../lib/oop");
    var BaseFoldMode = require("./fold_mode").FoldMode;

    var FoldMode = exports.FoldMode = function (markers) {
        this.foldingStartMarker = new RegExp("([\\[{])(?:\\s*)$|(" + markers + ")(?:\\s*)(?:#.*)?$");
    };
    oop.inherits(FoldMode, BaseFoldMode);

    (function () {

        this.getFoldWidgetRange = function (session, foldStyle, row) {
            var line = session.getLine(row);
            var match = line.match(this.foldingStartMarker);
            if (match) {
                if (match[1])
                    return this.openingBracketBlock(session, match[1], row, match.index);
                if (match[2])
                    return this.indentationBlock(session, row, match.index + match[2].length);
                return this.indentationBlock(session, row);
            }
        }

    }).call(FoldMode.prototype);

});

define("ace/mode/sbolshorthand2", ["require", "exports", "module", "ace/lib/oop", "ace/mode/text", "ace/mode/sbol_highlight_rules", "ace/mode/folding/sbolshorthandfold", "ace/range"], function (require, exports, module) {
    "use strict";

    var oop = require("../lib/oop");
    var TextMode = require("./text").Mode;
    var SBOLHighlightRules = require("./sbol_highlight_rules").SBOLHighlightRules;
    var SBOLFoldMode = require("./folding/sbolshorthandfold").FoldMode;
    var Range = require("../range").Range;

    var Mode = function () {
        this.HighlightRules = SBOLHighlightRules;
        this.foldingRules = new SBOLFoldMode("\\:");
    };
    oop.inherits(Mode, TextMode);

    (function () {

        this.lineCommentStart = "#";

        this.getNextLineIndent = function (state, line, tab) {
            var indent = this.$getIndent(line);

            var tokenizedLine = this.getTokenizer().getLineTokens(line, state);
            var tokens = tokenizedLine.tokens;

            if (tokens.length && tokens[tokens.length - 1].type == "comment") {
                return indent;
            }
            if (tokens.length && (tokens[tokens.length -1].type == prefixConstok || tokens[tokens.length -1].type == righteclipsetok) ) {
                return indent += tab;
            }

            return indent;
        };

        var outdents = {
            "": 1
        };

        this.checkOutdent = function (state, line, input) {
            if (input !== "\r\n" && input !== "\r" && input !== "\n")
                return false;

            var tokens = this.getTokenizer().getLineTokens(line.trim(), state).tokens;
            var last = tokens[tokens.length - 1];
            if (!last){
                return true;
            }

            return (outdents[last.value]);
        };

        this.autoOutdent = function (state, doc, row) {

            row += 1;
            var indent = this.$getIndent(doc.getLine(row));
            var tab = doc.getTabString();
            if (indent.slice(-tab.length) == tab)
                doc.remove(new Range(row, indent.length - tab.length, row, indent.length));
        };

        this.$id = "ace/mode/sbolshorthand2";
    }).call(Mode.prototype);

    exports.Mode = Mode;
});
