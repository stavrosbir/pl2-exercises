import sys
import requests

def extractNumber(s):
	indicator = '<span class="question">'
	index = s.find(indicator) + len(indicator)
	return s[index:(index+9)]

def stringToBinaryList(s):
	return map(int, list(bin(int(s))[2:]))

def writeProgram(s):
	binary = stringToBinaryList(s)
	first = ""
	second = ""
	for b in binary[1:]:
		if b == 1:
			first += ":"
			second += ":++"
		else:
			second += ":+"
	program = "!" + first + second + ".@"
	#check if it needs two lines
	length = len(program)
	if length>80:
		half = length/2
		up = program[:(half+1)] + "v"
		down = " "+program[:half:-1]+"<"
		return up+"\r\n"+down
	else:
		return program


def main():
	server = sys.argv[1]
	session = requests.Session()

	rounds = 0
	request = session.post(server)
	while rounds<10:
		number = extractNumber(request.text)
		print 'Round '+str(rounds+1)+', number '+number
		program = writeProgram(number)
		print 'Submitted solution:'
		print program
		request = session.post(server, data={'submit' : 'Submit!', 'program' : program})
		if 'class="right"' in request.text:
			print 'Right!  :-)'
			request = session.post(server, data={'again' : 'Play again!', 'reset' : 'reset'})
		else:
			print 'Wrong!  :-('
			request = session.post(server, data={'again' : 'Try again!'})
			request = session.post(server, data={'reset' : 'Change number!'})
		rounds += 1


main()
