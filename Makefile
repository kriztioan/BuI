PROGS:=bui
SRC_DIR:=src
CPP_FILES:=$(wildcard $(SRC_DIR)/*.cpp)
OBJ_FILES:=$(patsubst %.cpp,%.o,$(CPP_FILES))
DEP_FILES:=deps.d
CCPGPLOT_DIR=$(SRC_DIR)/CCPGPLOT
CCPGPLOT_LIB:=$(CCPGPLOT_DIR)/libccpgplot.a
CPPFLAGS:=-Wno-write-strings -I/opt/X11/include $(INC_PGPLOT) $(INC_READLINE) -MMD -MF $(DEP_FILES)
LIBS:=$(LIB_READLINE) -lreadline -L/opt/X11/lib -lX11 -lX11 $(LIB_PGPLOT) -lpgplot -lcpgplot $(LIB_EXTRA)

all: $(PROGS)

-include $(DEP_FILES)

$(PROGS): $(OBJ_FILES) $(CCPGPLOT_LIB)
	$(CXX) -o $(PROGS) $(OBJ_FILES) $(CCPGPLOT_LIB) $(LIBS) $(CPPFLAGS)

%.o: $(SRC_DIR)/%.cpp
	$(CXX) -c $< $(CPPFLAGS)

export $(INC_PGPLOT)
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
	$(RM) $(DEP_FILES) $(OBJ_FILES) $(PROGS)
