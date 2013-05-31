F03COMP=pgfortran
F03FLAGS= -g 

%.o : %.f03
	$(F03COMP) $(F03FLAGS) -c -o $@ $<

SRCS= \
    iterator.f03 \
    grid_coordinate.f03 \
    grid_coordinate_iterator.f03 \
    grid_iterator.f03 \
    grid.f03 \
    cell_iterator.f03
    

OBJS=$(SRCS:.f03=.o)

all: $(OBJS) tests
tests: test_cell_iterator
clean:
	rm -f *.o *.mod
test_cell_iterator: $(OBJS) test_cell_iterator.o
	$(F03COMP) $(F03FLAGS) -o test_cell_iterator \
		test_cell_iterator.o $(OBJS) 

grid_coordinate_iterator.o : grid_coordinate.o iterator.o
grid.o : grid_coordinate.o
cell_iterator.o : grid.o grid_coordinate_iterator.o iterator.o
