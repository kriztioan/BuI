/**
 *  @file   interplog.cpp
 *  @brief  Interpolation Tool (Log)
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

//#include "/usr/local/pgplot/cpgplot.h"
#include <fstream>
#include <iostream>
#include <cstdlib>
#include <string>
#include <cmath>

int main(int argc, char *argv[]) {

  // Program information

  if (argc != 4) {
    std::cerr << "Logarithmic Interpolation Program\n\nUsage:\n\t"
              << argv[0] << " <interpolationfile> <inputfile> <outputfile>\n\n";
    exit(1);
  }

  // Read in data file
  int fieldPointsNr = 0;
  std::string dummy;

  std::ifstream ifstr(argv[1], std::ios::in), inputfstr(argv[2], std::ios::in);
  while (std::getline(ifstr, dummy))
    ++fieldPointsNr;
  ifstr.close();
  ifstr.open(argv[1], std::ios::in);
  float Xarray[fieldPointsNr], Yarray[fieldPointsNr], read;
  for (int idx = 0; idx < fieldPointsNr; idx++) {
    ifstr >> read;
    if (read != 0)
      Xarray[idx] = std::log10(read);
    else
      Xarray[idx] = -38;

    ifstr >> read;
    if (read != 0)
      Yarray[idx] = std::log10(read);
    else
      Yarray[idx] = -38;
  }

  // Read in input file

  int interpolationNr = 0;
  while (inputfstr >> read)
    ++interpolationNr;
  inputfstr.close();
  inputfstr.open(argv[2], std::ios::in);

  // Interpolate input file

  float interpolate[interpolationNr], interpolated[interpolationNr];
  for (int idx = 0; idx < interpolationNr; idx++) {
    inputfstr >> read;
    interpolate[idx] = std::log10(read);

    int index = 0;
    while (interpolate[idx] > Xarray[index])
      ++index;

    if (interpolate[idx] == Xarray[index])
      interpolated[idx] = Yarray[index];
    else {
      float rc = (Yarray[index - 1] - Yarray[index]) /
                 (Xarray[index - 1] - Xarray[index]);
      interpolated[idx] =
          rc * interpolate[idx] + Yarray[index - 1] - rc * Xarray[index - 1];
    }
  }

  // Write interpolations to output file

  std::ofstream outputfstr(argv[3], std::ios::out);
  for (int idx = 0; idx < interpolationNr; idx++) {
    outputfstr.setf(std::ios::scientific);
    outputfstr << std::pow(10, interpolated[idx]) << '\n';
  }

  /*

    // Plot results to screen

    float xmin = Xarray[0], xmax = Xarray[0], ymin = Yarray[0], ymax =
    Yarray[0]; for (int idx = 1; idx < fieldPointsNr; idx++) { if (xmin >
    Xarray[idx]) xmin = Xarray[idx]; if (xmax < Xarray[idx]) xmax = Xarray[idx];
      if (ymin > Yarray[idx])
        ymin = Yarray[idx];
      if (ymin < Yarray[idx])
        ymin = Yarray[idx];
    }
    xmin = xmin - (xmax - xmin) * 0.1;
    xmax = xmax + (xmax - xmin) * 0.1;
    ymin = ymin - (ymax - ymin) * 0.1;
    ymax = ymax + (ymax - ymin) * 0.1;

    cpgbeg(0, "/XWINDOW", 1, 1);
    cpgscr(0, 1, 1, 1);
    cpgscr(1, 0, 0, 0);
    cpgenv(xmin, xmax, ymin, ymax, 0, 0);
    cpgslw(5);
    cpgsci(2);
    cpgpt(fieldPointsNr, Xarray, Yarray, 13);
    cpgslw(2);
    cpgsci(4);
    cpgsls(3);
    cpgline(fieldPointsNr, Xarray, Yarray);
    cpgslw(4);
    cpgsci(1);
    cpglab("Wavelength (\\gmm) LOG ->",
           "Incident Intensity (W . m-2 . s-1 . Hz-1) LOG ->",
           "External Radiation Field Properties");

    cpgslw(6);
    cpgsci(10);
    cpgpt(interpolationNr, interpolate, interpolated, 8);

    cpgend();
  */
  return 0;
}
