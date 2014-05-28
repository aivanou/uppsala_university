data=isolet.tar.gz
npoc=4
exec_file = kmeans
CCFLAGS = -o $(exec_file)
MPICC = mpic++
MPIRUN = mpirun
MPIRUN_FLAGS= -np 4
datafiles = mpi_input.bin_1 mpi_input.bin isolet.data 

all:
	tar xzvf $(data)
	$(MPICC) $(CCFLAGS) kmeans.c

run:
	$(MPIRUN) $(MPIRUN_FLAGS)  $(exec_file)

clean:
	rm -f $(datafiles) *.o
