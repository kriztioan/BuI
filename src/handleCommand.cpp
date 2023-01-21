/**
 *  @file   handleCommand.cpp
 *  @brief  Command Handlers Implementation
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#include "header.h"
#include <stdio.h>
extern "C" {
#include <readline/history.h>
#include <readline/readline.h>
}
extern ENV env;

void handleCommand(COM *com) {
  std::queue<std::string> parameter;

  if (env.script != NULL) {
    std::ifstream script(env.script, std::ios::in);
    if (script.good()) {
      std::string line;
      while (std::getline(script, line)) {
        if (line.length() > 0 && line.at(0) != '#') {
          com[interpedCommand(com, line, parameter)].function(parameter);
          while (!parameter.empty())
            parameter.pop();
        }
      }
    } else
      std::cerr << "Failed to open script '" << env.script << "'" << std::endl;
  }

  using_history();

  if (read_history(env.history_file) != 0)
    perror("read_history");

  char *line = NULL;
  while (true) {
    if (line) {
      free(line);
      line = NULL;
    }
    line = readline(env.prompt);
    if (line && *line) {
      add_history(line);
      while (!parameter.empty())
        parameter.pop();
      com[interpedCommand(com, std::string(line), parameter)].function(
          parameter);
    }
  }
}
