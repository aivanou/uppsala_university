import socket
import sys
import random
from multiprocessing import Process

HOST, PORT = "localhost", 3547



def run_multiple_clients(clients=5,infinite=False):
	processes=[]
	for i in xrange(0,clients):
		proc=Process(target=run_client)
		processes.append(proc)
	for i in xrange(0,clients):
		processes[i].start()
	for i in xrange(0,clients):
		processes[i].join()


def run_client(max_requests=100):
	for i in xrange(1,max_requests):	
		send_data(gen_data(1,30,20))

def send_data(data):
	sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	try:
    # Connect to server and send data
	    sock.connect(("localhost", PORT))
	    sock.sendall(data + "\n")
	    print sock.recv(5)
	    response = wait_response(sock)
	    print "Final response: ", response
    # Receive data from the server and shut down
    	
	finally:
		sock.close()


def wait_response(socket):
	answer="trying"
	while answer.strip()=="trying" or answer.strip()=="":
		answer=socket.recv(1024)
		if answer.strip() == "" :
			continue
		print "receive from server: ",answer
	return answer

def gen_data(MinVariable=1,MaxVariable=20,Expressions=15):
	data="[" + gen_expr(MinVariable,MaxVariable)
	for ind in xrange(1,Expressions):
		data+=","
		data+=gen_expr(MinVariable,MaxVariable)
	data+="]"
	return data

def gen_expr(MinVariable=1,MaxVariable=20):
	return "{" + \
		str(subs_if_zero(random.randint(-1*MaxVariable,MaxVariable))) + " , " + \
		str(subs_if_zero(random.randint(-1*MaxVariable,MaxVariable))) + " , " + \
		str(subs_if_zero(random.randint(-1*MaxVariable,MaxVariable))) + \
	"}"


def subs_if_zero(variable):
	if variable==0: 
		return 1
	else: 
		return variable

if __name__ == '__main__':
    # run_client()
    # send_data(gen_data())
    run_multiple_clients()
    # print gen_data(1,20,40)


# hard:

# [{26,-24,29},
#                   {9,7,22},
#                   {7,-16,-10},
#                   {13,-27,-26},
#                   {-7,1,-19},
#                   {-5,-2,-3},
#                   {13,23,1},
#                   {-18,-23,-26},
#                   {9,30,11},
#                   {-10,-6,-10},
#                   {3,15,-8},
#                   {30,10,-12},
#                   {29,2,-4},
#                   {-18,-27,11},
#                   {12,-19,27},
#                   {25,28,1},
#                   {-20,5,6},
#                   {-25,21,-28},
#                   {14,-19,-14},
#                   {-15,26,-3}]