from flask import Flask, request
from subprocess import check_output, CalledProcessError


app = Flask(__name__)


@app.route('/befunge93-api/', methods=['POST'])
def befunge93():
    try:
    	program = request.data
    	request.args['restrict']

    except KeyError:
    	pass

    else:
    	if not restricted(program):
    		return 'Program contains illegal commands', 400

    return execute(program)



def execute(program):
	file = open('program.bf', 'w')
	file.write(program)
	file.close()

	try:
		output = check_output(['timeout', '1', './befunge_interpreter', 'program.bf'])

	except CalledProcessError:
		return 'Execution timed out', 401

	else:
		return output, 200



def restricted(program):
	restricted_chars = '0123456789?"pg&~'
	for rc in restricted_chars:
		if rc in program:
			return False
	return True


def main():
    app.run()


main()
