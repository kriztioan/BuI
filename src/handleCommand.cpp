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

int com_cmp(const void *str, const void *com) {
  return strcmp(*(char **)str, ((COM *)com)->name);
}

int arg_cmp(const void *str1, const void *str2) {
  return strcmp(*(char **)str1, *(char **)str2);
}

void arg_sep(const char *str, size_t &len, char *buff, char **args,
             size_t size) {

  len = 0;
  if (buff)
    free(buff);
  buff = strdup(str);
  for (char **ptr = args; (*ptr = strsep(&buff, " ")) != NULL;)
    if (**ptr != '\0') {
      ++len;
      if (++ptr >= args + size)
        break;
    }
}

char *arg_generator(const char *text, int state) {

  static char *line_buffer = NULL, *arg_buffer = NULL, *arg_list[16], *list[16];
  static size_t list_index, list_len, text_len, arg_len;

  if (!state) {
    arg_sep(rl_line_buffer, arg_len, line_buffer, arg_list, 16);

    list_len = N_COMMANDS;
    COM *com = (COM *)lfind(arg_list, rl_com, &list_len, sizeof(COM), com_cmp);
    if (!com)
      return NULL;

    arg_sep(com->arguments, list_len, arg_buffer, list, 16);

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
