/**
 *  @file   main.cpp
 *  @brief  BuI
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#include "header.h"

#define HELP_FILE "share/bui/bui.hlp"
#define LANGUAGE_FILE "share/bui/language.lan"
#define INPUT_FILE "share/bui/default/default.in"

ENV env = {.version = "0.9",
           .year = {0},
           .help_file = NULL,
           .help_idx = "bui.idx",
           .language_file = NULL,
           .language_idx = "language.idx",
           .history_file = "history.bui",
           .prompt = "[BuI] ",
           .script = NULL,
           .t_start = 0,
           .history_lines = 40};
WIN win = {-1, -1};
STYLE style = {0, 0, 0, '\0'};
IO io = {NULL, NULL, NULL, NULL};
COM *com = NULL;
MES *mes = NULL;
MOD *mod = NULL;

int main(int argc, char *argv[], char *envp[]) {

  // years

  time_t t = time(NULL);
  struct tm *tm_s = localtime(&t);
  size_t n = strftime(env.year, sizeof(env.year), "2004/%Y", tm_s);

  // checking for arguments

  if (argc != 1) {
    std::string file(argv[1]);
    if (file == "-h" || file == "help")
      usage(argv[0]);

    env.script = argv[1];
  }

  // environment

  const char *buidir = getenv("BUIDIR");
  if (buidir) {
    size_t n = strlen(buidir) + 1 + strlen(HELP_FILE) + 1;
    env.help_file = (char *)malloc(n);
    snprintf(env.help_file, n, "%s/%s", buidir, HELP_FILE);
    n = strlen(buidir) + 1 + strlen(LANGUAGE_FILE) + 1;
    env.language_file = (char *)malloc(n);
    snprintf(env.language_file, n, "%s/%s", buidir, LANGUAGE_FILE);
  } else {
    size_t n = strlen(HELP_FILE) + 1;
    env.help_file = (char *)malloc(n);
    strncpy(env.help_file, HELP_FILE, n);
    n = strlen(LANGUAGE_FILE) + 1;
    env.language_file = (char *)malloc(n);
    strncpy(env.language_file, LANGUAGE_FILE, n);
  }

  // setting some defaults

  if(buidir) {
    size_t n = strlen(buidir) + 1 + strlen(INPUT_FILE) + 1;
    io.input_file = new char[n];
    snprintf(io.input_file, n, "%s/%s", buidir, INPUT_FILE);
  } else {
    size_t n = strlen(buidir) + 1;
    io.input_file = new char[n];
    strncpy(io.input_file, INPUT_FILE, n);
  }

  io.output_file = strcpy(new char[strlen("default.out") + 1], "default.out");
  io.plot_file = strcpy(new char[strlen("default.ps") + 1], "default.ps");
  io.data_file = strcpy(new char[strlen("default.dat") + 1], "default.dat");

  // initializing language

  if (initializeFile(env.language_idx, env.language_file)) {
    std::cerr << "Language initilization failed ... Aborting\n";
    exit(1);
  }

  // setting memory handler

  std::set_new_handler(memoryError);

  // loading messages

  mes = new MES[N_MESSAGES];

  if (!loadLanguageMessages(mes)) {
    std::cerr << "Loading Language failed ... Aborting\n";
    exit(1);
  }

  std::cout << std::endl;
  message(STARTUP);
  if (getenv("HOST") != NULL)
    std::cout << std::string("Running @ ") + getenv("HOST") << std::endl;
  message(SETMEMORYHANDLER);
  message(SETENVIORMENTCONSTANTS);
  message(INITIALIZEDLANGUAGE);

  // loading model

  if (loadInputfile(io.input_file, &mod)) {
    message(FAILEDINPUTFILE);
    exit(1);
  }

  if (argc > 1)
    message(INPUTFILELOADED);
  else
    message(DEFAULTSLOADED);

  // loading commands

  com = new COM[N_COMMANDS];
  if (loadLanguageCommands(com))
    message(LOADEDCOMMANDS);
  else {
    message(FAILEDLOADCOMMANDS);
    exit(1);
  }

  // initializing help

  message(INITIALIZEHELP);
  if (!initializeFile(env.help_idx, env.help_file))
    message(INITIALIZEDHELP);
  else {
    message(FAILINITIALIZEHELP);
    exit(1);
  }

  // setting command function

  message(SETTINGCOMMANDFUNCTIONS);

  loadCommands(com);

  message(SETCOMMANDFUNCTIONS);

  message(INITIALIZEPGPLOT);

  if (initializePglot())
    message(INITIALIZEDPGPLOT);
  else {
    message(FAILEDINITIALIZEPGPLOT);
    exit(1);
  }

  // loading language help

  if (loadLanguageHelp(com))
    message(LOADEDHELP);
  else {
    message(FAILEDLOADHELP);
    exit(1);
  }

  // loading interface

  loadInterface();

  // command handeling

  handleCommand(com);

  // wrong exit!

  return (1);
}
