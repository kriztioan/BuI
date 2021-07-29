/**
 *  @file   usage.cpp
 *  @brief  Usage Implementation
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#include "header.h"

extern ENV env;

void usage(char const *programName) {
  std::string programNameString(programName);
  std::string::size_type beginPos, endPos;

  beginPos = programNameString.find_last_of("\\");
  if (beginPos != std::string::npos)
    programNameString.erase(0, beginPos + 1);

  endPos = programNameString.find_last_of(".");
  if (endPos != std::string::npos)
    programNameString.erase(endPos, programNameString.size() - endPos);

  for (int pos = 0; pos < programNameString.size(); pos++)
    programNameString.at(pos) = tolower(programNameString.c_str()[pos]);

  std::cout << "BUI Version " << env.version << " Christiaan Boersma "
            << env.year << std::endl
            << "\nUser Interface to CSDUST3" << std::endl
            << std::endl
            << "usage:" << std::endl
            << programNameString << " [-h|help] [script]" << std::endl
            << std::endl
            << "[-h|-help]\tthis screen" << std::endl
            << "[script]\tscript file with commands to run" << std::endl
            << std::endl;

  exit(0);
}
