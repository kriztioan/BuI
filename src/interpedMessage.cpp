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
  if (strcasecmp(compare, "STARTUP") == 0)
    return (STARTUP);
  else if (strcasecmp(compare, "SETMEMORYHANDLER") == 0)
    return (SETMEMORYHANDLER);
  else if (strcasecmp(compare, "SETENVIORMENTCONSTANTS") == 0)
    return (SETENVIORMENTCONSTANTS);
  else if (strcasecmp(compare, "INITIALIZEDLANGUAGE") == 0)
    return (INITIALIZEDLANGUAGE);
  else if (strcasecmp(compare, "INITIALIZEHELP") == 0)
    return (INITIALIZEHELP);
  else if (strcasecmp(compare, "FAILINITIALIZEHELP") == 0)
    return (FAILINITIALIZEHELP);
  else if (strcasecmp(compare, "INITIALIZEDHELP") == 0)
    return (INITIALIZEDHELP);
  else if (strcasecmp(compare, "LOADEDHELP") == 0)
    return (LOADEDHELP);
  else if (strcasecmp(compare, "FAILEDLOADHELP") == 0)
    return (FAILEDLOADHELP);
  else if (strcasecmp(compare, "LOADEDCOMMANDS") == 0)
    return (LOADEDCOMMANDS);
  else if (strcasecmp(compare, "FAILEDLOADCOMMANDS") == 0)
    return (FAILEDLOADCOMMANDS);
  else if (strcasecmp(compare, "SETTINGCOMMANDFUNCTIONS") == 0)
    return (SETTINGCOMMANDFUNCTIONS);
  else if (strcasecmp(compare, "SETCOMMANDFUNCTIONS") == 0)
    return (SETCOMMANDFUNCTIONS);
  else if (strcasecmp(compare, "INITIALIZEUI") == 0)
    return (INITIALIZEUI);
  else if (strcasecmp(compare, "ONEXIT") == 0)
    return (ONEXIT);
  else if (strcasecmp(compare, "DEFAULTSLOADED") == 0)
    return (DEFAULTSLOADED);
  else if (strcasecmp(compare, "INPUTFILELOADED") == 0)
    return (INPUTFILELOADED);
  else if (strcasecmp(compare, "FAILEDINPUTFILE") == 0)
    return (FAILEDINPUTFILE);
  else if (strcasecmp(compare, "INITIALIZEPGPLOT") == 0)
    return (INITIALIZEPGPLOT);
  else if (strcasecmp(compare, "INITIALIZEDPGPLOT") == 0)
    return (INITIALIZEDPGPLOT);
  else if (strcasecmp(compare, "FAILEDINITIALIZEPGPLOT") == 0)
    return (FAILEDINITIALIZEPGPLOT);
  else if (strcasecmp(compare, "WRITEHISTORY") == 0)
    return (WRITEHISTORY);
  else if (strcasecmp(compare, "FREEMEMORY") == 0)
    return (FREEMEMORY);
  else if (strcasecmp(compare, "ENDPGPLOT") == 0)
    return (ENDPGPLOT);
  else {
    std::cerr << "An error occured while interpeting message (" << compare
              << ")... Aborting" << std::endl;
    exit(1);
  }
}
