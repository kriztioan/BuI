/**
 *  @file   12to9.cpp
 *  @brief  Tool for Clipping Characters 5-7
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#include <iostream>
#include <string>

int main() {
  std::string line;
  while (std::getline(std::cin, line)) {
    line.erase(5, 3);
    std::cout << line << '\n';
  }

  return 0;
}
