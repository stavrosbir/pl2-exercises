from subprocess import call
import os


files = os.listdir('testCases/')

for file in files:
	print file
	call(['./befunge_interpreter', 'testCases/'+file])
