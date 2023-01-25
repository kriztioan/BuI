/**
 *  @file   human_readable.cpp
 *  @brief  Human Readable Implementation
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#include "human_readable.h"

std::string human_readable(size_t bytes) {

  const char *units[] = {" bytes", " kiB", " MiB", " GiB", " TiB"};

  float raw = static_cast<float>(bytes);

  unsigned int i = 0;
  while (raw > 1024.0f) {
    raw /= 1024.0f;
    ++i;
  }

  std::stringstream ss;

  ss << std::fixed << std::setprecision(i) << raw << units[i];

  return ss.str();
}