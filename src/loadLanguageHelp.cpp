/**
 *  @file   loadLanguageHelp.cpp
 *  @brief  Load Language Help Implementation
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#include "header.h"

extern ENV env;

bool loadLanguageHelp(COM *com) {
  std::ifstream index(env.help_idx, std::ios::in);

  std::string compare;
  char character;

  do {
    compare.erase();
    do {
      index.get(character);
      if (index.fail())
        return (true);
    } while (character != '[');
    compare += character;

    while (character != ']') {
      index.get(character);
      compare += character;
      if (index.fail())
        return (true);
    }

    index.read(
        (char *)&com[interpedKey(const_cast<char *>(compare.c_str()))].offset,
        sizeof(long));
  } while (index.good());

  return (false);
}
