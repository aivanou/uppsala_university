CC=gcc
LIBS=-lm 
FLAGS= -O3 -pthread
pi: pi.c 
	$(CC) $(FLAGS) -o pi pi.c $(LIBS)
	
data: data.c 
	$(CC) $(FLAGS) -o data data.c $(LIBS)	
	
synch: synch.c 
	$(CC) $(FLAGS) -o synch synch.c $(LIBS)
		
spinwait: spinwait.c 
	$(CC) $(FLAGS) -o spinwait spinwait.c $(LIBS)
			
helloworld: helloworld.c
	$(CC) $(FLAGS) -o helloworld helloworld.c
	
hello2: hello_arg2.c
	$(CC) $(FLAGS) -o hello2 hello_arg2.c

join: join.c
	$(CC) $(FLAGS) -o join join.c $(LIBS)
	
matmul: matmul.c
	$(CC) $(FLAGS) -o matmul matmul.c time.c $(LIBS)	
	
lu: lu.c
	$(CC) $(FLAGS) -o lu lu.c time.c $(LIBS)
	
mutex: mutex.c
	$(CC) $(FLAGS) -o mutex mutex.c $(LIBS)	

enumsort: enumsort.c
	$(CC) $(FLAGS) -o enumsort enumsort.c time.c $(LIBS)	

clean:
	rm -f *.o *~ helloworld hello2 data mutex join enumsort \
	synch spinwait pi matmul lu


