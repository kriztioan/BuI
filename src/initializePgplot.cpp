/**
 *  @file   initializePgplot.cpp
 *  @brief  PGPlot initialization Implementation
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#include "header.h"

#include <X11/Xlib.h>

extern WIN win;
extern STYLE style;
extern ENV env;

int initializePglot() {
  Display *display;
  Window window;

  display = XOpenDisplay(0);

  char *device = (char *)malloc(sizeof(char) * (strlen(getenv("DISPLAY")) + 9));

  device = strcpy(device, getenv("DISPLAY"));

  device = strcat(device, "/XWINDOW");

  win.graphicsID = cpgopen(device);

  win.windowSize = 10.0;

  cpgpap(win.windowSize, 0.9);
  cpgscr(0, 1, 1, 1);
  cpgscr(1, 0, 0, 0);

  cpgsci(4);
  cpgscf(2);
  cpgsch(10);
  cpgslw(10);
  cpgswin(0, 10, 0, 10);
  cpgpage();
  cpgtext(1, 6, "BuI");
  cpgsci(2);
  cpgscf(3);
  cpgsch(8);
  cpgslw(8);
  cpgtext(2.5, 4, env.version);
  cpgslw(1);
  cpgsch(1);
  cpgscf(1);
  cpgsci(1);
  cpgtext(5.6, 0, "Christiaan Boersma");
  cpgtext(8.5, 0, env.year);

  cpgask(false);

  style.symbol = 5;
  style.color = 2;
  style.lineweight = 5;
  style.type = 'l';

  return (win.graphicsID);
}
