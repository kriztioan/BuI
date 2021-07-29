/**
 *  @file   e2d.cpp
 *  @brief  Convert e to d Tool
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#include <iostream>

int main() {
  char character;
  while (std::cin.get(character)) {
    if (character == 'e')
      std::cout << 'd';
    else
      std::cout << character;
  }

  return 0;
}
