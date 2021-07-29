/**
 *  @file   grid.cpp
 *  @brief  Wavelength Grid Making Tool
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#include <cmath>
#include <iomanip>
#include <iostream>

int main() {

  int NR = 100;
  std::cout.setf(std::ios::scientific);
  std::cout.precision(3);

  // float ground = 10, zeroCrossing = log10(0.2229) / log10(ground);
  float power = 2.1, start = 0.2229;

  for (int idx = 0; idx < NR; idx++)
    std::cout << ((1 - start) / pow(NR - 1, power)) * pow(idx, power) + start
              << '\n';

  // cout << pow(ground, -idx * (zeroCrossing / 99) + zeroCrossing) << endl;

  // cout << exp(-idx*0.025010320386 - 1.5010320386)<< endl;

  // cout << (0.08 / 0.35) + (((1 - (0.08 / 0.35)) / 99) * idx) << endl;

  //  NR = 100 - NR;
  // for(int idx = 0; idx < NR; idx++)
  //       cout << (0.2229 + (100 - NR) * STEP) + idx * (1 - (0.2229 + (100 -
  //       NR) * STEP)) / NR << endl;

  return 0;
}
