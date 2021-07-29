/**
 *  @file   conv.cpp
 *  @brief  Tool to do Convolution
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#include <fstream>
#include <iostream>
#include <cmath>
#include <cstdlib>
#include <string>

#define PI 3.141592654
#define D 2.3e3
#define R 0.35
#define X 0.01

int main(int argc, char *argv[]) {

  // Program information

  if (argc != 4) {
    std::cerr << "Convolution Calculation Program\n\nUsage:\n\t" << argv[0]
              << " <sourcefile> <wavelengthfile> <outputfile>\n\n";
    exit(1);
  }

  double SIZE = atan(R / D) * 3600 * 180 / PI;
 
  // Read in source file

  int fieldPointsNr = 0, fieldPointsNr2 = 0;
  std::string dummy;

  std::ifstream ifstr(argv[1], std::ios::in), gridifstr(argv[2], std::ios::in);
  while (std::getline(ifstr, dummy))
    fieldPointsNr++;
  ifstr.close();
  ifstr.open(argv[1], std::ios::in);

  float wavelengthSource[fieldPointsNr], BW[fieldPointsNr];

  for (int idx = 0; idx < fieldPointsNr; idx++) {
    ifstr >> wavelengthSource[idx];
    ifstr >> BW[idx];
    BW[idx] = BW[idx] / (SIZE * sqrt(8 * log(2)));
  }

  // read in wavelength file

  while (std::getline(gridifstr, dummy))
    ++fieldPointsNr2;
  gridifstr.close();
  gridifstr.open(argv[2], std::ios::in);

  float wavelength[fieldPointsNr2];

  for (int idx = 0; idx < fieldPointsNr2; idx++)
    gridifstr >> wavelength[idx];

  // write output file

  std::ofstream ofstr(argv[3], std::ios::out);
  ofstr.setf(std::ios::scientific);
  ofstr.precision(3);
  for (int idx = 0; idx < fieldPointsNr2; idx++) {
    bool foundflag = false;
    for (int index = 0; index < fieldPointsNr; index++) {
      if (wavelength[idx] == wavelengthSource[index]) {
        foundflag = true;
        std::cout << idx << " " << wavelength[idx] << " found " << BW[index] << '\n';
        ofstr << BW[index] << '\n';
        break;
      }
    }

    if (!foundflag)
      ofstr << X / (SIZE * sqrt(8 * log(2))) << '\n';
  }

  return 0;
}
