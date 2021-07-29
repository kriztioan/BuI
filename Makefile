PROG:=bui
SRC_DIR:=src
CPP_FILES:=$(wildcard $(SRC_DIR)/*.cpp)
OBJ_FILES:=$(patsubst %.cpp,%.o,$(CPP_FILES))
CCPGPLOT_DIR=$(SRC_DIR)/CCPGPLOT
CCPGPLOT_LIB:=$(CCPGPLOT_DIR)/libccpgplot.a
CPPFLAGS:=-g -Wno-write-strings
LIBS:=-lpgplot -lcpgplot -lhistory -lreadline -lX11

$(PROG): $(OBJ_FILES) $(CCPGPLOT_LIB)
	$(CXX) -o $(PROG) $(OBJ_FILES) $(CCPGPLOT_LIB) $(LIBS)

%.o: $(SRC_DIR)/%.cpp
	$(CXX) -c $< $(CPPFLAGS)

$(CCPGPLOT_LIB):
	$(MAKE) -C $(CCPGPLOT_DIR)

.PHONY: csdust3
csdust3:
	$(MAKE) -C csdust3

.PHONY: tools
tools:
	$(MAKE) -C tools

clean:
	$(MAKE) -C tools clean
	$(MAKE) -C csdust3 clean
	$(MAKE) -C $(CCPGPLOT_DIR) clean
	$(RM) $(SRC_DIR)/*.o $(PROG)
