/**
 *  @file   interpredKey.cpp
 *  @brief  Interpred Key Implementation
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#include "header.h"

command interpedKey(char *key) {

  size_t len = strlen(key) - 2;

  if (strncmp(key + 1, "EXIT", len) == 0)
    return (EXIT);
  else if (strncmp(key + 1, "DISPLAY", len) == 0)
    return (DISPLAY);
  else if (strncmp(key + 1, "LIST", len) == 0)
    return (LIST);
  else if (strncmp(key + 1, "HELP", len) == 0)
    return (HELP);
  else if (strncmp(key + 1, "SAVE", len) == 0)
    return (SAVE);
  else if (strncmp(key + 1, "INFO", len) == 0)
    return (INFO);
  else if (strncmp(key + 1, "UNKNOWN", len) == 0)
    return (UNKNOWN);
  else if (strncmp(key + 1, "RETURN", len) == 0)
    return (RETURN);
  else if (strncmp(key + 1, "RUN", len) == 0)
    return (RUN);
  else if (strncmp(key + 1, "CONTOUR", len) == 0)
    return (CONTOUR);
  else if (strncmp(key + 1, "PLOT", len) == 0)
    return (PLOT);
  else if (strncmp(key + 1, "CREATE", len) == 0)
    return (CREATE);
  else if (strncmp(key + 1, "SET", len) == 0)
    return (SET);
  else if (strncmp(key + 1, "SHOW", len) == 0)
    return (SHOW);
  else if (strncmp(key + 1, "SHELL", len) == 0)
    return (SHELL);
  else if (strncmp(key + 1, "BUI_WINDOW", len) == 0)
    return (BUI_WINDOW);
  else if (strncmp(key + 1, "GRAPH", len) == 0)
    return (GRAPH);
  else {
    std::cerr << "An error occured while interpeting key (" << key
              << ")... Aborting" << std::endl;
    return (UNKNOWN);
  }
}
