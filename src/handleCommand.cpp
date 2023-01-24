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
    std::cout << "Running script '" << env.script << "'." << std::endl;
    std::ifstream script(env.script, std::ios::in);
    if (script.good()) {
      int line_number = 0;
      std::string line;
      while (std::getline(script, line)) {
        ++line_number;
        if (line.length() > 0 && line.at(0) != '#') {
          if (com[interpedCommand(com, line, parameter)].function(parameter) !=
              0) {
            std::cerr << "Error while running '" << env.script
                      << "'. Stopped.\n\t" << line_number << ": \001\033[31;1m"
                      << line << "\001\033[0;0m" << std::endl;
            break;
          }
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
