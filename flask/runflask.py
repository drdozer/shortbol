from flask import Flask, render_template, request, redirect, url_for, abort, session
app = Flask(__name__)

@app.route('/')
def home():
    return render_template('index.html')

@app.route('/documentation')
def documentation():
    return render_template('documentation.html')
if __name__ == '__main__':
    app.run("0.0.0.0",debug=True, threaded=True)
