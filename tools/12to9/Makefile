PROG:=../12to9.bin
CPP_FILES:=$(wildcard *.cpp)
OBJ_FILES:=$(patsubst %.cpp,%.o,$(CPP_FILES))
CPPFLAGS:=-O2

$(PROG): $(OBJ_FILES)
	$(CXX) -o $(PROG) $(notdir $(OBJ_FILES))

%.o: %.cpp
	$(CXX) -c $< $(CPPFLAGS)

clean:
	$(RM) *.o $(PROG)
	