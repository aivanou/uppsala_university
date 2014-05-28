LIBS=-lmpi
COMPILE=mpicc
FLAGS= -O3 

wave: bound.o force.o diffop.o wave.o residual.o initcomm.o wave.h 
	$(COMPILE) $(FLAGS) -o wave wave.o bound.o force.o diffop.o \
	residual.o initcomm.o $(LIBS)

wave.o: wave.c wave.h
	$(COMPILE) $(FLAGS) -c wave.c

bound.o: bound.c wave.h 
	$(COMPILE) $(FLAGS) -c bound.c

force.o: force.c wave.h
	$(COMPILE) $(FLAGS) -c force.c

diffop.o: diffop.c wave.h
	$(COMPILE) $(FLAGS) -c diffop.c

residual.o: residual.c wave.h
	$(COMPILE) $(FLAGS) -c residual.c

initcomm.o: initcomm.c wave.h
	$(COMPILE) $(FLAGS) -c initcomm.c

clean:
	rm -f wave *.o *~ PDE.out
