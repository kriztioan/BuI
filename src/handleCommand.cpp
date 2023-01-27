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
#include <search.h>
}
extern ENV env;

COM *rl_com;

char *com_generator(const char *text, int state) {

  static int list_index, len;

  if (!state) {
    list_index = 0;
    len = strlen(text);
  }

  char *name;
  while (list_index < N_COMMANDS) {
    name = rl_com[list_index++].name;
    if (strncmp(name, text, len) == 0) {
      return (strdup(name));
    }
  }

  return NULL;
}

int arg_cmp(const void *str1, const void *str2) {

  return strcmp(*(char **)str1, *(char **)str2);
}

char *arg_generator(const char *text, int state) {

  static char *line_buffer = NULL, *arg_buffer = NULL, *arg_list[16], *list[16];
  static size_t list_index, list_len, text_len, arg_len;

  if (!state) {
    arg_len = 0;
    if (line_buffer)
      free(line_buffer);
    line_buffer = strdup(rl_line_buffer);
    for (char **arg_p = arg_list; (*arg_p = strsep(&line_buffer, " ")) != NULL;)
      if (**arg_p != '\0') {
        ++arg_len;
        if (++arg_p >= &arg_list[16])
          break;
      }

    list_len = 0;
    for (int i = 0; i < N_COMMANDS; i++) {
      if (strcmp(arg_list[0], rl_com[i].name) == 0) {
        if (arg_buffer)
          free(arg_buffer);
        arg_buffer = strdup(rl_com[i].arguments);
        for (char **arg_p = list; (*arg_p = strsep(&arg_buffer, " ")) != NULL;)
          if (**arg_p != '\0') {
            ++list_len;
            if (++arg_p >= &list[16])
              break;
          }
      }
    }

    list_index = 0;
    text_len = strlen(text);
  }

  char *name;
  while (list_index < list_len) {
    name = list[list_index++];
    if (strncmp(name, text, text_len) == 0 &&
        NULL == lfind(&name, arg_list, &arg_len, sizeof(char **), arg_cmp)) {
      return (strdup(name));
    }
  }

  return NULL;
}

char **bui_completion(const char *text, int start, int end) {

  rl_compentry_func_t *comp_func = start == 0 ? com_generator : arg_generator;

  return rl_completion_matches(text, comp_func);
}

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

  rl_com = com;

  rl_readline_name = "BuI";

  rl_attempted_completion_function = (rl_completion_func_t *)bui_completion;

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
