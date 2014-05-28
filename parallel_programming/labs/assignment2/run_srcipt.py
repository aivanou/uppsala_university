#!/usr/bin/python2.7
import sys
import matplotlib.pyplot as plt
from subprocess import Popen, PIPE
import matplotlib.image as mpimg

def_algs=["builtin","dnc","peer"]
def_sizes=["2048","10000","1000000"]
def_cores=["1","2","4"]

def run_programm(params):
	process = Popen(params, stdout=PIPE)
	(output, err) = process.communicate()
	exit_code = process.wait()	
	return output

def parse_output(output):
	return float((output.split("\n")[0]).split(":")[1].strip())


def run(algs,sizes,cores):
	output_data=[]
	for alg in algs:
		for size in sizes:
			for core in cores:
				time=parse_output(run_programm(["./a.out","-t"+core,"-s"+size,alg]))
				out=dict()
				out["size"]=size
				out["alg"]=alg
				out["core"]=core
				out["time"]=time
				output_data.append(out)
	return output_data


def process(filename,sizes,cores,xlabel,ylabel):
	output_data=run(def_algs,sizes,cores)
	pdata = parse_for_plot(output_data,xlabel,ylabel)
	f=open(filename+".txt",'w')
	for k,v in pdata.items():
		f.write(k+"\n")
		print v
		x,y = zip(*v)
		for i in x:
			f.write(str(i)+" ")
		f.write("\n")
		for i in y:
			f.write(str(i)+" ")
		f.write("\n")
	f.close()
	# write_plot(pdata,filename,xlabel,ylabel)

def parse_file(filename):
	f=open(filename,'r')
	data=dict()
	for i in range(0,4):
		alg=f.readline().strip()
		x=map(int,f.readline().strip().split(' '))
		y=map(float,f.readline().strip().split(' '))
		data[alg]=[x,y]
	return data



def parse_for_plot(data,xlabel,ylabel):
	pdata=dict()
	for d in data:
		if d["alg"] not in pdata:
			pdata[d["alg"]]=[]
		pdata[d["alg"]].append((d[xlabel],d[ylabel]))
	return pdata 

def write_plot(data,filename,xlabel,ylabel):

	colors=['y-','r-','g-','b-']
	fig, ax = plt.subplots()
	ind=0
	for k,v in data.items():
		x,y=v
		ax.plot(x,y,colors[ind],label=k)
		ind+=1
	ax.set_xlabel(xlabel)
	ax.set_ylabel(ylabel)
	legend = ax.legend(loc='upper left', shadow=False)
	frame = legend.get_frame()

	# plt.show()
	fig.savefig(filename)

def main():
	write_plot(parse_file("size_time.png.txt"),"size_time.png","size","time(seconds)")
	write_plot(parse_file("core_time.png.txt"),"core_time.png","core","time(seconds)")
	# process("core_time.png",["1000000"],["1","2","4"],"core","time")
	# process("size_time.png",["1000","10000","10000","10000","10000"],["1","2","4"],"size","time")


if __name__=="__main__":
	main()