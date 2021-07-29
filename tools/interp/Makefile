CPP_FILES:=$(wildcard *.cpp)
OBJ_FILES:=$(patsubst %.cpp,%.o,$(CPP_FILES))
PROGS:=$(patsubst %.cpp,../%.bin,$(CPP_FILES))
CPPFLAGS:=-O2

all: $(PROGS)

$(PROGS): ../%.bin : %.o
	$(CXX) -o $@ $< $(CPPFLAGS)

$(OBJ_FILES): %.o : %.cpp 
	$(CXX) -c $< $(CPPFLAGS)

clean:
	$(RM) *.o $(PROGS)
