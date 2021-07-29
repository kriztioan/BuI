/**
 *  @file   dust.cpp
 *  @brief  Tool for Calculating Dust Properties
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#include <fstream>
#include <iostream>
#include <cmath>
#include <string>

int main(int argc, char *argv[]) {

  // Program information

  if (argc != 6) {
    std::cerr << "Dust Properties Caluclation Program\n\nUsage:\n\t" << argv[0]
              << " <extinctionfile> <albedofile> <gfile> <wavelengthfile> "
                 "<outputfile>\n\n";
    exit(1);
  }

  // Read in data file
  int fieldPointsNr = 0;
  std::string dummy;

  std::ifstream ifstr(argv[1], std::ios::in), albifstr(argv[2], std::ios::in),
      gifstr(argv[3], std::ios::in), wavelengthfstr(argv[4], std::ios::in);
  while (std::getline(ifstr, dummy))
    ++fieldPointsNr;
  ifstr.close();
  ifstr.open(argv[1], std::ios::in);

  float extinction[fieldPointsNr], albedo[fieldPointsNr],
      absorption[fieldPointsNr], scattering[fieldPointsNr], g[fieldPointsNr],
      wavelength[fieldPointsNr];

  for (int idx = 0; idx < fieldPointsNr; idx++) {
    ifstr >> extinction[idx];
    albifstr >> albedo[idx];
    gifstr >> g[idx];
    wavelengthfstr >> wavelength[idx];

    absorption[idx] = extinction[idx] / (1.0f + albedo[idx]);
    scattering[idx] = absorption[idx] * albedo[idx];
  }

  // Write output file

  std::ofstream ofstr(argv[5], std::ios::out);
  ofstr.setf(std::ios::scientific);
  ofstr.precision(3);
  for (int idx = 0; idx < fieldPointsNr; idx++) {
    ofstr << absorption[idx] << " " << scattering[idx] << " " << g[idx] << " "
          << wavelength[idx] << '\n';
  }

  return 0;
}
