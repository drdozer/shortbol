from flask import Flask, render_template, request, redirect, url_for, abort, session
app = Flask(__name__)
from templatehandler import TemplateHandler
@app.route('/')
def home():
    return render_template('index.html')

@app.route('/documentation')
def documentation():
    # s = Sequences()
    # cd = ComponentDefinitions()
    # sa = SequenceAnnotations()
    # pa = Participations()
    # inter = Interactions()
    # md = ModuleDefinitions()
    # pc = PropertyConstructors()
    #
    # templates = {}
    # templates["Sequences"] = s
    # templates["ComponentDefinitions"] = cd
    # templates["SequenceAnnotations"] = sa
    # templates["Participations"] = pa
    # templates["Interactions"] = inter
    # templates["ModuleDefinitions"] = md
    # templates["PropertyConstructors"] = pc

    th = TemplateHandler()

    return render_template('documentation.html',templates=th)
if __name__ == '__main__':
    app.run("0.0.0.0",port=5000,debug=True, threaded=True)
