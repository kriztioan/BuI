/**
 *  @file   createIdx.cpp
 *  @brief  Index Creation Implementation
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#include "header.h"

bool createIdx(char const *indexFile, char const *dataFile) {
  std::ifstream data(dataFile, std::ios::in);
  std::ofstream index(indexFile, std::ios::out);

  char character;
  long offset = 0;

  while (true) {
    data.get(character);
    ++offset;
    while (character != '[') {
      data.get(character);
      ++offset;
      if (data.eof())
        return (true);
      if (character == ']')
        return (false);
    }
    index.put(character);
    do {
      data.get(character);
      ++offset;
      if (data.eof() || data.fail() || character == '[')
        return (false);
      index.put(character);
    } while (character != ']');

    index.write((char *)(&offset), sizeof(long));
    if (data.fail())
      return (false);
  }
}
