/**
 *  @file   initializeFile.cpp
 *  @brief  File Initialization Implementation
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#include "header.h"

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

bool initializeFile(char const *indexFile, char const *dataFile) {

  struct stat stat_idx, stat_data;

  int idx = stat(indexFile, &stat_idx), data = stat(dataFile, &stat_data);

  if (data == -1)
    return (true);
  else if (idx == 0) {
    if (stat_idx.st_mtime < stat_data.st_mtime)
      return (!createIdx(indexFile, dataFile));
    else
      return (false);
  } else
    return (!createIdx(indexFile, dataFile));
}
