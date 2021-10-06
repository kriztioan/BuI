/**
 *  @file   loadInterface.cpp
 *  @brief  Load Interface Implementation
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#include "header.h"
#include "human_readable.h"
#include <stdio.h>
#include <sys/sysinfo.h>

extern ENV env;

void loadInterface() {
  std::string user("");
  if (getenv("USER") != NULL)
    user = getenv("USER");
  user = " " + user;

  struct tm stm;
  time(&env.t_start);
  stm = *localtime(&env.t_start);
  char *asc_time = asctime(&stm);
  asc_time[strlen(asc_time) - 1] = '\0';

  struct sysinfo s_info;
  int error = sysinfo(&s_info); // error not used!

  std::cout << "Loading interface \001\033[90;96m\n\n"
            << "+--------------------------------------------------------------"
               "---------------+\n"
            << "|                                                              "
               "               |\n"
            << "|   ******    **   ***   ***    Version " << env.version
            << "    Christiaan Boersma " << env.year << "   |\n"
            << "|   **  ***   **   ***   ***                                   "
               "               |\n"
            << "|   *******   **   ***                                         "
               "               |\n"
            << "|   *****     **   ***   ***                                   "
               "               |\n"
            << "|   **  ***   ********   ***                                   "
               "               |\n"
            << "|   *******   ********   ***                                   "
               "               |\n"
            << "|   ******     ******    ***    "
               "https://github.com/kriztioan/bui.git          |\n"
            << "|   " << asc_time
            << "                                                  |\n"
            << "|                                                              "
               "               |\n"
            << "+--------------------------------------------------------------"
               "---------------+\001\033[0;0m\n\n"
            << "Memory status : RAM     : total "
            << human_readable(s_info.totalram) << " / free "
            << human_readable(s_info.freeram) << " / shared "
            << human_readable(s_info.sharedram)
            << "\n                BUFFERS : total "
            << human_readable(s_info.bufferram)
            << "\n                SWAP    : total "
            << human_readable(s_info.totalswap) << " / free "
            << human_readable(s_info.freeswap) << "\n\nInterface loaded\n\n"
            << "Welcome" << user
            << ", type a command or try 'help' or 'list'\n";
}
