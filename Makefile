EXE = number_theory_tools_test
ADA_VERSION = -gnat05
SRC = Number_Theory_Tools_Test.adb
FLAGS = -gnato -gnatwa -fstack-check
AR = ar
LIB_SRC = Number_Theory_Tools.adb 
OBJ_OUT = Number_Theory_Tools.o 
LIB = libnumber_theory.a
LIB_FLAGS = rcs


all: test

obj:
	gnatmake -c $(LIB_SRC) $(FLAGS) -o $(OBJ_OUT)

lib: obj
	$(AR) $(LIB_FLAGS) $(LIB) $(OBJ_OUT) 


test: lib
	gnatmake $(FLAGS) -L. -ltarga $(SRC) -o $(EXE)

ada83: 
	gnatmake -gnat83 $(FLAGS) $(SRC)

ada95: 
	gnatmake -gnat95 $(FLAGS) $(SRC)

ada2005: 
	gnatmake -gnat05 $(FLAGS) $(SRC)

ada2012: 
	gnatmake -gnat12 $(FLAGS) $(SRC)

clean:
	rm *.ali *~ *.o *.a *.tga $(EXE)
