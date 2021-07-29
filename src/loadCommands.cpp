/**
 *  @file   loadCommands.cpp
 *  @brief  Load Commands Implementation
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#include "header.h"

void loadCommands(COM *com) {
  com[EXIT].function = doexit;
  com[DISPLAY].function = display;
  com[LIST].function = list_commands;
  com[HELP].function = help;
  com[SAVE].function = save;
  com[INFO].function = info;
  com[CONTOUR].function = contour;
  com[UNKNOWN].function = unknown;
  com[RETURN].function = return_func;
  com[SET].function = set;
  com[RUN].function = run;
  com[PLOT].function = writeplot;
  com[SHOW].function = show;
  com[CREATE].function = create;
  com[SHELL].function = shell;
  com[BUI_WINDOW].function = window;
  com[GRAPH].function = graph;
}
