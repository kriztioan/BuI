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

#ifdef __APPLE__
#include <mach/mach.h>
#include <mach/mach_host.h>
#include <sys/sysctl.h>
#else
#include <sys/sysinfo.h>
#endif

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

#ifdef __APPLE__
  mach_port_t mach = mach_host_self();
  vm_size_t page_size;
  host_page_size(mach, &page_size);
  mach_msg_type_number_t num = HOST_VM_INFO_COUNT;
  vm_statistics_data_t vm;
  host_statistics(mach, HOST_VM_INFO, (host_info_t)&vm, &num);
  size_t freeram = vm.free_count * page_size;
  size_t sharedram = vm.active_count * page_size;
  size_t bufferram = vm.inactive_count * page_size;
  size_t totalram;
  size_t len = sizeof(totalram);
  sysctlbyname("hw.memsize", &totalram, &len, NULL, 0);
  struct xsw_usage si;
  len = sizeof(si);
  sysctlbyname("vm.swapusage", &si, &len, NULL, 0);
  size_t totalswap = si.xsu_total;
  size_t freeswap = si.xsu_avail;
#else
  struct sysinfo s_info;
  sysinfo(&s_info);
  size_t freeram = s_info.freeram;
  size_t sharedram = s_info.sharedram;
  size_t bufferram = s_info.bufferram;
  size_t totalram = s_info.totalram;
  size_t totalswap = s_info.totalswap;
  size_t freeswap = s_info.freeswap;
#endif

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
            << "Memory status : RAM     : total " << human_readable(totalram)
            << " / free " << human_readable(freeram) << " / shared "
            << human_readable(sharedram) << "\n                BUFFERS : total "
            << human_readable(bufferram) << "\n                SWAP    : total "
            << human_readable(totalswap) << " / free "
            << human_readable(freeswap) << "\n\nInterface loaded\n\n"
            << "Welcome" << user
            << ", type a command or try 'help' or 'list'\n";
}
