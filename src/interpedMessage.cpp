/**
 *  @file   interpredMessage.cpp
 *  @brief  Interpred Message Implementation
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#include "header.h"

messages interpedMessage(char *key) {
  char *compare = key + 1;
  *(compare + strlen(compare) - 1) = '\0';
  if (strcmp(compare, "STARTUP") == 0)
    return (STARTUP);
  else if (strcmp(compare, "SETMEMORYHANDLER") == 0)
    return (SETMEMORYHANDLER);
  else if (strcmp(compare, "SETENVIORMENTCONSTANTS") == 0)
    return (SETENVIORMENTCONSTANTS);
  else if (strcmp(compare, "INITIALIZEDLANGUAGE") == 0)
    return (INITIALIZEDLANGUAGE);
  else if (strcmp(compare, "INITIALIZEHELP") == 0)
    return (INITIALIZEHELP);
  else if (strcmp(compare, "FAILINITIALIZEHELP") == 0)
    return (FAILINITIALIZEHELP);
  else if (strcmp(compare, "INITIALIZEDHELP") == 0)
    return (INITIALIZEDHELP);
  else if (strcmp(compare, "LOADEDHELP") == 0)
    return (LOADEDHELP);
  else if (strcmp(compare, "FAILEDLOADHELP") == 0)
    return (FAILEDLOADHELP);
  else if (strcmp(compare, "LOADEDCOMMANDS") == 0)
    return (LOADEDCOMMANDS);
  else if (strcmp(compare, "FAILEDLOADCOMMANDS") == 0)
    return (FAILEDLOADCOMMANDS);
  else if (strcmp(compare, "SETTINGCOMMANDFUNCTIONS") == 0)
    return (SETTINGCOMMANDFUNCTIONS);
  else if (strcmp(compare, "SETCOMMANDFUNCTIONS") == 0)
    return (SETCOMMANDFUNCTIONS);
  else if (strcmp(compare, "INITIALIZEUI") == 0)
    return (INITIALIZEUI);
  else if (strcmp(compare, "ONEXIT") == 0)
    return (ONEXIT);
  else if (strcmp(compare, "DEFAULTSLOADED") == 0)
    return (DEFAULTSLOADED);
  else if (strcmp(compare, "INPUTFILELOADED") == 0)
    return (INPUTFILELOADED);
  else if (strcmp(compare, "FAILEDINPUTFILE") == 0)
    return (FAILEDINPUTFILE);
  else if (strcmp(compare, "INITIALIZEPGPLOT") == 0)
    return (INITIALIZEPGPLOT);
  else if (strcmp(compare, "INITIALIZEDPGPLOT") == 0)
    return (INITIALIZEDPGPLOT);
  else if (strcmp(compare, "FAILEDINITIALIZEPGPLOT") == 0)
    return (FAILEDINITIALIZEPGPLOT);
  else if (strcmp(compare, "WRITEHISTORY") == 0)
    return (WRITEHISTORY);
  else if (strcmp(compare, "FREEMEMORY") == 0)
    return (FREEMEMORY);
  else if (strcmp(compare, "ENDPGPLOT") == 0)
    return (ENDPGPLOT);
  else {
    std::cerr << "An error occured while interpeting message (" << compare
              << ")... Aborting" << std::endl;
    exit(1);
  }
}
