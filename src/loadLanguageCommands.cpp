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

bool loadLanguageCommands(COM *com) {
  std::ifstream index(env.language_idx, std::ios::in),
      data(env.language_file, std::ios::in);

  std::string compare, name, description;
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

    name.erase();
    data.get(character);
    while (character != '{') {
      data.get(character);
      if (character == '>' || character == '}')
        return (false);
    }
    name += character;
    do {
      data.get(character);
      if (data.eof() || data.fail() || character == '{')
        return (false);
      name += character;
    } while (character != '}');

    description.erase();
    while (character != '{') {
      data.get(character);
      if (character == '>' || character == '}')
        return (false);
    }
    description += character;
    do {
      data.get(character);
      if (data.eof() || data.fail() || character == '{')
        return (false);
      description += character;
    } while (character != '}');

    command key = interpedKey(const_cast<char *>(compare.c_str()));
    com[key].key = key;
    com[key].name =
        strcpy(new char[name.substr(1, name.length() - 2).length() + 1],
               name.substr(1, name.length() - 2).c_str());
    com[key].description = strcpy(
        new char[description.substr(1, description.length() - 2).length() + 1],
        description.substr(1, description.length() - 2).c_str());
  }
}
