from flask import Flask, render_template, request, redirect, url_for, abort, session
app = Flask(__name__)
from templatehandler import TemplateHandler
from templates import ExampleCode

TH = TemplateHandler()

@app.route('/')
def home():
    return render_template('index.html')

@app.route('/documentation')
def documentation():
    th = TemplateHandler()
    examplecode = ExampleCode()
    ec = examplecode.code
    return render_template('documentation.html',templates=TH,examplecode=ec)


@app.route('/editor')
def editor():
    jsonw = open("static/json/templateexamples.json","w")
    jsonw.write('[')
    templatedict = TH.__dict__
    templatedictkeys = list(templatedict.keys())
    jsonw.write('{"Template":' + '"' + list(templatedict[templatedictkeys[0]].getExamples().keys())[0] + '"}')
    i = 0
    while i < len(templatedictkeys):
        templateobj = templatedict[templatedictkeys[i]]
        examples = templateobj.getExamples()
        keys = list(examples.keys())
        j = 0
        while (j<len(examples)):
            jsonw.write(',{"Template":' + '"' + keys[j] + '"}')
            j += 1
        i += 1;

    jsonw.write("]")

    return render_template('editor.html')

if __name__ == '__main__':

    app.run("0.0.0.0",port=5000,debug=True, threaded=True)
