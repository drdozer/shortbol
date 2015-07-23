/**
 * Created by chris on 23/07/15.
 */
define("ace/mode/python_highlight_rules",["require","exports","module","ace/lib/oop","ace/mode/text_highlight_rules"], function(require, exports, module) {
//"use strict";

var oop = require("../lib/oop");
var TextHighlightRules = require("./text_highlight_rules").TextHighlightRules;

var PythonHighlightRules = function() {

    var keywords = (
        "as|at|import|"
    );

    //var builtinConstants = (
    //    "True|False|None|NotImplemented|Ellipsis|__debug__"
    //);

    //var builtinFunctions = (
    //    "abs|divmod|input|open|staticmethod|all|enumerate|int|ord|str|any|" +
    //    "eval|isinstance|pow|sum|basestring|execfile|issubclass|print|super|" +
    //    "binfile|iter|property|tuple|bool|filter|len|range|type|bytearray|" +
    //    "float|list|raw_input|unichr|callable|format|locals|reduce|unicode|" +
    //    "chr|frozenset|long|reload|vars|classmethod|getattr|map|repr|xrange|" +
    //    "cmp|globals|max|reversed|zip|compile|hasattr|memoryview|round|" +
    //    "__import__|complex|hash|min|set|apply|delattr|help|next|setattr|" +
    //    "buffer|dict|hex|object|slice|coerce|dir|id|oct|sorted|intern"
    //);
    var keywordMapper = this.createKeywordMapper({
        "invalid.deprecated": "debugger",
        //"support.function": builtinFunctions,
        //"constant.language": builtinConstants,
        "keyword": keywords
    }, "identifier");

    var strPre = "(?:r|u|ur|R|U|UR|Ur|uR)?";

    var decimalInteger = "(?:(?:[1-9]\\d*)|(?:0))";
    //var octInteger = "(?:0[oO]?[0-7]+)";
    //var hexInteger = "(?:0[xX][\\dA-Fa-f]+)";
    //var binInteger = "(?:0[bB][01]+)";
    var integer = "(?:" + decimalInteger + ")";

    //var exponent = "(?:[eE][+-]?\\d+)";
    //var fraction = "(?:\\.\\d+)";
    //var intPart = "(?:\\d+)";
    //var pointFloat = "(?:(?:" + intPart + "?" + fraction + ")|(?:" + intPart + "\\.))";
    //var exponentFloat = "(?:(?:" + pointFloat + "|" +  intPart + ")" + exponent + ")";
    //var floatNumber = "(?:" + exponentFloat + "|" + pointFloat + ")";

    var keywords = ["as","at","import"];
    var keywordsregex = "";

    for(var i =0; i < keywords.length; i++){
        if ((keywords.length - 1) == i ){
            keywordsregex += keywords[i];

        }
        else{
            keywordsregex += keywords[i] + "|";

        }
    }
    var stringEscape =  "\\\\(x[0-9A-Fa-f]{2}|[0-7]{3}|[\\\\abfnrtv'\"]|U[0-9A-Fa-f]{8}|u[0-9A-Fa-f]{4})";

    this.$rules = {
        "start" : [
            {
                token: "keyword.colonstate",
                regex: /(^|):/,         // :
                next : "sboltemp"
        }, {
                token: "keyword.rightarrowstate",
                regex: /(^|)=>/,
                next : "sboltemp"
        },
            {
            token : "comment",
            regex : /#[^\n]+/
        }, {
                token: function(word1, word2, word3){

                    var word2token = "support.function.sboltemplate";
                    var index = keywords.indexOf(word2);
                    if (index != -1){
                        word2token = "keyword"
                    }
                    var tokenarray = ["nothing",word2token,"nothing"];
                    return tokenarray
                },
                regex:/([a-zA-Z_][a-zA-Z0-9_]*[\s\t]+)((?!hede)[a-z_][a-z0-9_]*)([\s\t]+[a-zA-Z_][a-zA-Z0-9_]*)/

        },{
            token : "keyword",
            regex : keywordsregex  // /\b(as|at|import)\b/
        },{
            token : "string",           //multi line string.
            regex : strPre + '{',
            next : "qqstring3"
        }, {
            token : "string",           // " string
            regex : strPre + '"(?=.)',
            next : "qqstring"
        },{
            token : "string",           // ' string
            regex : strPre + "'(?=.)",
            next : "qstring"
        },
         {
            token : "constant.numeric", // integer
            regex : integer + "\\b"
        },  {
            token : "keyword.operator",
            regex : ":|=>|="
        }, {
                token: "entity.other.sbolresource",
                regex: /<.*>/
        },
         ],
        "sboltemp" : [ {
            token : "support.function.sboltemplate",
            regex : /[\s\t]*[a-zA-Z_][a-zA-Z0-9_]*/,
            next : "start"
        }, {
            defaultToken : "commastate"
        } ],
        "qqstring3" : [ {
            token : "constant.language.escape",
            regex : stringEscape
        }, {
            token : "string",
            regex : '}',
            next : "start"
        }, {
            defaultToken : "string"
        } ],
        "qqstring" : [{
            token : "constant.language.escape",
            regex : stringEscape
        }, {
            token : "string",
            regex : "\\\\$",
            next  : "qqstring"
        }, {
            token : "string",
            regex : '"|$',
            next  : "start"
        }, {
            defaultToken: "string"
        }],
        "qstring" : [{
            token : "constant.language.escape",
            regex : stringEscape
        }, {
            token : "string",
            regex : "\\\\$",
            next  : "qstring"
        }, {
            token : "string",
            regex : "'|$",
            next  : "start"
        }, {
            defaultToken: "string"
        }]
    };

};

oop.inherits(PythonHighlightRules, TextHighlightRules);

exports.PythonHighlightRules = PythonHighlightRules;
});

define("ace/mode/folding/pythonic",["require","exports","module","ace/lib/oop","ace/mode/folding/fold_mode"], function(require, exports, module) {
"use strict";

var oop = require("../../lib/oop");
var BaseFoldMode = require("./fold_mode").FoldMode;

var FoldMode = exports.FoldMode = function(markers) {
    this.foldingStartMarker = new RegExp("([\\[{])(?:\\s*)$|(" + markers + ")(?:\\s*)(?:#.*)?$");
};
oop.inherits(FoldMode, BaseFoldMode);

(function() {

    this.getFoldWidgetRange = function(session, foldStyle, row) {
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

define("ace/mode/sbolshorthand",["require","exports","module","ace/lib/oop","ace/mode/text","ace/mode/python_highlight_rules","ace/mode/folding/pythonic","ace/range"], function(require, exports, module) {
"use strict";

var oop = require("../lib/oop");
var TextMode = require("./text").Mode;
var PythonHighlightRules = require("./python_highlight_rules").PythonHighlightRules;
var PythonFoldMode = require("./folding/pythonic").FoldMode;
var Range = require("../range").Range;

var Mode = function() {
    this.HighlightRules = PythonHighlightRules;
    this.foldingRules = new PythonFoldMode("\\:");
};
oop.inherits(Mode, TextMode);

(function() {

    this.lineCommentStart = "#";

    this.getNextLineIndent = function(state, line, tab) {
        var indent = this.$getIndent(line);

        var tokenizedLine = this.getTokenizer().getLineTokens(line, state);
        var tokens = tokenizedLine.tokens;

        if (tokens.length && tokens[tokens.length-1].type == "comment") {
            return indent;
        }

        if (state == "start") {
            var match = line.match(/^.*[\{\(\[\:]\s*$/);
            if (match) {
                indent += tab;
            }
        }

        return indent;
    };

    var outdents = {
        "pass": 1,
        "return": 1,
        "raise": 1,
        "break": 1,
        "continue": 1
    };

    this.checkOutdent = function(state, line, input) {
        if (input !== "\r\n" && input !== "\r" && input !== "\n")
            return false;

        var tokens = this.getTokenizer().getLineTokens(line.trim(), state).tokens;

        if (!tokens)
            return false;
        do {
            var last = tokens.pop();
        } while (last && (last.type == "comment" || (last.type == "text" && last.value.match(/^\s+$/))));

        if (!last)
            return false;

        return (last.type == "keyword" && outdents[last.value]);
    };

    this.autoOutdent = function(state, doc, row) {

        row += 1;
        var indent = this.$getIndent(doc.getLine(row));
        var tab = doc.getTabString();
        if (indent.slice(-tab.length) == tab)
            doc.remove(new Range(row, indent.length-tab.length, row, indent.length));
    };

    this.$id = "ace/mode/sbolshorthand";
}).call(Mode.prototype);

exports.Mode = Mode;
});
