/**
 *  @file   header.h
 *  @brief  BuI Header
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#ifndef HEADER_H_
#define HEADER_H_

#include <fstream>
#include <iomanip>
#include <iostream>
#include <istream>
#include <sstream>

#include <string>

#include <new>

#include <queue>

#include <cpgplot.h>

#include "CCPGPLOT/CCdraw.h"

#include "commandFunctions.h"

#include <cmath>
#include <cstdlib>
#include <cstring>
#include <ctime>

struct ENV {
  char *version;
  char year[10];
  char *help_file;
  char *help_idx;
  char *language_file;
  char *language_idx;
  char *history_file;
  char *prompt;
  char *script;

  time_t t_start;

  int history_lines;
};

struct WIN {
  float windowSize;
  int graphicsID;
};

struct IO {
  char *output_file;
  char *input_file;
  char *plot_file;
  char *data_file;
};

struct STYLE {
  int symbol;
  int color;
  int lineweight;
  char type;
};

typedef enum {
  EXIT,
  DISPLAY,
  LIST,
  HELP,
  SAVE,
  INFO,
  RUN,
  SET,
  SHELL,
  PLOT,
  SHOW,
  CREATE,
  CONTOUR,
  UNKNOWN,
  BUI_WINDOW,
  GRAPH,
  RETURN,
  N_COMMANDS
} command;

struct COM {
  char *name;
  char *description;

  command key;

  long offset;

  void (*function)(std::queue<std::string> &parameter);
};

typedef enum {
  STARTUP,
  SETMEMORYHANDLER,
  SETENVIORMENTCONSTANTS,
  INITIALIZEDLANGUAGE,
  INITIALIZEHELP,
  FAILINITIALIZEHELP,
  LOADEDHELP,
  FAILEDLOADHELP,
  FAILEDINPUTFILE,
  INITIALIZEDHELP,
  LOADEDCOMMANDS,
  FAILEDLOADCOMMANDS,
  SETTINGCOMMANDFUNCTIONS,
  SETCOMMANDFUNCTIONS,
  INITIALIZEUI,
  DEFAULTSLOADED,
  INPUTFILELOADED,
  INITIALIZEPGPLOT,
  INITIALIZEDPGPLOT,
  FAILEDINITIALIZEPGPLOT,
  WRITEHISTORY,
  FREEMEMORY,
  ENDPGPLOT,
  ONEXIT,
  N_MESSAGES
} messages;

struct MES {
  char *text;

  messages key;
};

typedef enum {
  NMODEL,
  IGEOM,
  IEMRG,
  IEDFTR,
  IOUT,
  IC,
  NH,
  ISCA,
  IB,
  IDIST,
  AJ0,
  EPS,
  ITMAX,
  NFD,
  IOF,
  IMIX,
  IREADT,
  TDS,
  TDC,
  RMAX,
  TAU0F,
  RHOCS,
  TSTAR,
  TLUM,
  ICONV,
  N_PARAMETERS
} parameter;

struct MOD {
  float spatialgrid[100];
  float wavelengthgrid[100];
  float interstellar[60];
  float convolution[60];
  float dust[5][60][4];
  float abundances[5];
  float radii[5][2];
  float type[5];

  char *dustname[5][2];
  char *parameters[N_PARAMETERS][2];
};

void memoryError();
void usage(char const *program);

bool initializeFile(char const *indexFile, char const *dataFile);
bool createIdx(char const *indexFile, char const *dataFile);

messages interpedMessage(char *key);
command interpedKey(char *key);
command interpedCommand(COM *com, std::string input,
                        std::queue<std::string> &parameter);

void message(messages key);

bool loadLanguageMessages(MES *mes);
bool loadLanguageCommands(COM *com);
bool loadLanguageHelp(COM *com);

void loadCommands(COM *com);

typedef enum { INPUTFILE, OUTPUTFILE, PLOTFILE, DATAFILE } globalParameters;

void loadInterface();
void handleCommand(COM *com);

int initializePglot();

int loadInputfile(const char *file, MOD **mod);

#endif // End of HEADER_H_
