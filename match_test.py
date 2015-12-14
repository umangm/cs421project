
import os

#os.system("echo 'let f = 5;;' | tailrec/picomlIntPar > 'result.txt'")

os.system("./tailrec/tailRecPicoMLTest > './tailrec_result.txt' ")

os.system("./tailrecCPS/tailRecPicoMLTest > './tailrecCPS_result.txt' ")


f = open('tailrec_result.txt', 'r')
lines = f.readlines()

f_cps = open('tailrecCPS_result.txt', 'r')
lines_cps = f_cps.readlines()

if len(lines) != len(lines_cps):
	print("Numbers of Test cases don't match. ")
else:
	for i in range(len(lines)):
		if lines[i] != lines_cps[i]:
			
			print("\nTest Case #", str(int((i+1)/2)), "has unmatched results: ", )

			print("From tailrec/tailRecPicoMLTest: ")
			if i-1 >= 0: 
				print(lines[i-1])
			print(lines[i])

			print("From tailrecCPS/tailRecPicoMLTest: ")
			if i-1 >= 0: 
				print(lines_cps[i-1])
			print(lines_cps[i])

