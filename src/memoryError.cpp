/**
 *  @file   memoryError.cpp
 *  @brief  Memory Error Implementation
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#include "header.h"

void memoryError() {
  std::cerr << "Memory allocation fault... Aborting" << std::endl;
  exit(1);
}
