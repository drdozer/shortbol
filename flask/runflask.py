from flask import Flask, render_template, request, redirect, url_for, abort, session
app = Flask(__name__)
from templatehandler import TemplateHandler
from templates import ExampleCode
@app.route('/')
def home():
    return render_template('index.html')

@app.route('/documentation')
def documentation():
    th = TemplateHandler()
    examplecode = ExampleCode()
    ec = examplecode.code
    return render_template('documentation.html',templates=th,examplecode=ec)


@app.route('/editor')
def editor():
    return render_template('editor.html')

if __name__ == '__main__':
    app.run("0.0.0.0",port=5000,debug=True, threaded=True)
