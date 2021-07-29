/**
 *  @file   loadLanguageMessages.cpp
 *  @brief  Load Language Messages Implementation
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#include "header.h"

extern struct ENV env;

bool loadLanguageMessages(MES *mes) {
  std::ifstream index(env.language_idx, std::ios::in),
      data(env.language_file, std::ios::in);

  std::string compare, message;
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
  } while (compare != "[MESSAGES]");

  long offset;
  index.read((char *)&offset, sizeof(long));
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

    message.erase();
    data.get(character);
    while (character != '{') {
      data.get(character);
      if (character == '>' || character == '}')
        return (false);
    }
    message += character;
    do {
      data.get(character);
      if (data.eof() || data.fail() || character == '{')
        return (false);
      message += character;
    } while (character != '}');
    messages key = interpedMessage(const_cast<char *>(compare.c_str()));
    mes[key].key = key;
    mes[key].text = strcpy(new char[message.length() - 1],
                           message.substr(1, message.length() - 2).c_str());
  }
}
