/**
 *  @file   interpretCommand.cpp
 *  @brief  Interpret Command Implementation
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#include "header.h"

command interpedCommand(COM *com, std::string input,
                        std::queue<std::string> &parameter) {

  if (input.empty())
    return (RETURN);
  std::string::size_type begin, end;
  begin = input.find_first_not_of(" \t");
  if (begin == std::string::npos)
    return (RETURN);
  while (begin != std::string::npos) {
    if (input.at(begin) == '\"') {
      end = input.find_first_of("\"", begin + 1);
      if (end != std::string::npos)
        end++;
      else
        end = input.size();
    } else
      end = input.find_first_of(" \t", begin);
    if (end == std::string::npos)
      parameter.push(input.substr(begin));
    else
      parameter.push(input.substr(begin, end - begin));
    begin = input.find_first_not_of(" \t", end);
  }
  const char *name = parameter.front().c_str();
  parameter.pop();
  for (int i = 0; i < N_COMMANDS; i++) {
    if (strcmp(com[i].name, name) == 0)
      return (com[i].key);
  }
  return (UNKNOWN);
}
