/**
 *  @file   aline.cpp
 *  @brief  Tool for Reformating to 8-Column Data
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#include <iostream>
#include <string>

int main() {
  std::string line;
  int lineNr = 0;
  while (std::getline(std::cin, line)) {
    ++lineNr;
    if (lineNr == 8) {
      lineNr = 1;
      std::cout << '\n' << line << " ";
    } else
      std::cout << line << " ";
  }
  return 0;
}
