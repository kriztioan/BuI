/**
 *  @file   loadLanguageCommands.cpp
 *  @brief  Load Language Commands Implementation
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#include "header.h"

extern ENV env;

bool sec_parse(std::ifstream &in, std::string &out) {

  out.erase();

  char c;
  in.get(c);
  while (c != '{') {
    in.get(c);
    if (in.eof() || c == '>' || c == '}')
      return (false);
  }
  out += c;
  do {
    in.get(c);
    if (in.eof() || in.fail() || c == '{')
      return (false);
    out += c;
  } while (c != '}');

  return(true);
}

bool loadLanguageCommands(COM *com) {
  std::ifstream index(env.language_idx, std::ios::in),
      data(env.language_file, std::ios::in);

  std::string compare, name, description, arguments;
  char character;

  do {
    compare.erase();
    do {
      index.get(character);
    } while (character != '[');
    compare += character;

    while (character != ']') {
      index.get(character);
      compare += character;
    }
  } while (compare != "[COMMANDS]");

  long offset;
  index.read((char *)(&offset), sizeof(long));
  data.ignore(offset);
  data.clear();

  while (true) {

    compare.erase();
    data.get(character);
    while (character != '<') {
      data.get(character);
      if (data.eof() || character == '[')
        return (true);
      if (character == '>')
        return (false);
    }
    compare += character;
    do {
      data.get(character);
      if (data.eof() || data.fail() || character == '<')
        return (false);
      compare += character;
    } while (character != '>');

    if(!sec_parse(data, name))
      return(false);

    if(!sec_parse(data, description))
      return(false);

    if(!sec_parse(data, arguments))
      return(false);

    command key = interpedKey(const_cast<char *>(compare.c_str()));
    com[key].key = key;
    com[key].name =
        strcpy(new char[name.substr(1, name.length() - 2).length() + 1],
               name.substr(1, name.length() - 2).c_str());
    com[key].description = strcpy(
        new char[description.substr(1, description.length() - 2).length() + 1],
        description.substr(1, description.length() - 2).c_str());

    com[key].arguments = strcpy(
        new char[arguments.substr(1, arguments.length() - 2).length() + 1],
        arguments.substr(1, arguments.length() - 2).c_str());
  }
}
