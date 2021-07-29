/**
 *  @file   message.cpp
 *  @brief  Message Implementation
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#include "header.h"

extern MES *mes;

void message(messages key) {
  std::cout << mes[key].text << std::endl;

  return;
}
