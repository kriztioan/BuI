/**
 *  @file   loadInputfile.cpp
 *  @brief  Load Inputfile Implementation
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#include "header.h"

extern IO io;

int loadInputfile(const char *, MOD **mod) {

  std::ifstream ifstr(io.input_file, std::ios::in);
  if (ifstr.fail())
    return (1);

  int models = 0;
  if (*mod != NULL) {
    models = atoi(mod[0]->parameters[NMODEL][1]);
    for (int i = 0; i < models; i++) {

      for (int j = 0; j < atoi(mod[i]->parameters[IMIX][1]); j++) {
        delete mod[i]->dustname[j][0];
        delete mod[i]->dustname[j][1];
      }
      for (int k = 1; k < N_PARAMETERS; k++) {
        delete mod[i]->parameters[k][0];
        delete mod[i]->parameters[k][1];
      }
    }
  }
  delete *mod;

  std::string line, n_model, n_model_value;

  std::getline(ifstr, n_model, '=');
  ifstr >> n_model_value;
  *mod = new MOD[atoi(n_model_value.c_str())];

  int nmodels = atoi(n_model_value.c_str());
  for (int models = 0; models < nmodels; models++) {

    mod[models]->parameters[NMODEL][0] =
        strcpy(new char[n_model.length() + 1], n_model.c_str());
    mod[models]->parameters[NMODEL][1] =
        strcpy(new char[n_model_value.length() + 1], n_model_value.c_str());
    for (int dummy = 1; dummy < 10; dummy++) {
      getline(ifstr, line, '=');
      mod[models]->parameters[dummy][0] =
          strcpy(new char[line.length() + 1], line.c_str());
      ifstr >> line;
      mod[models]->parameters[dummy][1] =
          strcpy(new char[line.length() + 1], line.c_str());
    }

    ifstr >> line;
    line.at(5) = 'e';
    mod[models]->parameters[AJ0][0] =
        strcpy(new char[strlen("AJO") + 1], "AJO");
    mod[models]->parameters[AJ0][1] =
        strcpy(new char[line.length() + 1], line.c_str());

    ifstr >> line;
    line.at(5) = 'e';
    mod[models]->parameters[EPS][0] =
        strcpy(new char[strlen("EPS") + 1], "EPS");
    mod[models]->parameters[EPS][1] =
        strcpy(new char[line.length() + 1], line.c_str());

    ifstr >> line;
    mod[models]->parameters[ITMAX][0] =
        strcpy(new char[strlen("ITMAX") + 1], "ITMAX");
    mod[models]->parameters[ITMAX][1] =
        strcpy(new char[line.length() + 1], line.c_str());

    int index = 0;
    for (int dummy = 0; dummy < 100; dummy++) {
      ifstr >> line;
      line.at(5) = 'e';
      mod[models]->spatialgrid[dummy] = atof(line.c_str());
    }

    ifstr >> line;
    mod[models]->parameters[NFD][0] =
        strcpy(new char[strlen("NFD") + 1], "NFD");
    mod[models]->parameters[NFD][1] =
        strcpy(new char[line.length() + 1], line.c_str());

    int number_of_wavelength_points = atoi(line.c_str()),
        number_of_lines = int(ceil(atof(line.c_str()) / 7.));

    ifstr >> line;
    mod[models]->parameters[IOF][0] =
        strcpy(new char[strlen("IOF") + 1], "IOF");
    mod[models]->parameters[IOF][1] =
        strcpy(new char[line.length() + 1], line.c_str());

    std::getline(ifstr, line);
    index = 0;
    for (int dummy = 0; dummy < number_of_lines; dummy++) {
      std::getline(ifstr, line);
      std::string::size_type begin = line.find_first_of("dD");
      while (begin < line.length()) {
        line.at(begin) = 'e';
        if (line.at(begin + 1) == ' ')
          line.at(begin + 1) = '+';
        mod[models]->wavelengthgrid[index++] =
            atof((line.substr(begin - 5, 9)).c_str());
        begin = line.find_first_of("dD", begin + 1);
      }
    }
    index = 0;
    for (int dummy = 0; dummy < number_of_lines; dummy++) {
      getline(ifstr, line);

      std::string::size_type begin = line.find_first_of("dD");
      while (begin < line.length()) {
        line.at(begin) = 'e';
        if (line.at(begin + 1) == ' ')
          line.at(begin + 1) = '+';
        mod[models]->interstellar[index++] =
            atof((line.substr(begin - 5, 9)).c_str());
        begin = line.find_first_of("dD", begin + 1);
      }
    }

    ifstr >> line;
    mod[models]->parameters[IMIX][0] =
        strcpy(new char[strlen("IMIX") + 1], "IMIX");
    mod[models]->parameters[IMIX][1] =
        strcpy(new char[line.length() + 1], line.c_str());

    int number_of_dust_models = atoi(line.c_str());
    getline(ifstr, line);
    for (int model = 0; model < number_of_dust_models; model++) {
      ifstr >> line;
      mod[models]->dustname[model][0] =
          strcpy(new char[line.length() + 1], line.c_str());
      ifstr >> line;
      line.at(5) = 'e';
      mod[models]->radii[model][0] = atof(line.c_str());

      ifstr >> line;
      ifstr >> line;
      if (line.find_first_of("dD") != std::string::npos)
        line.at(5) = 'e';
      mod[models]->radii[model][1] = atof(line.c_str());
      mod[models]->dustname[model][1] =
          strcpy(new char[line.length() + 1], line.c_str());

      ifstr >> line;
      mod[models]->type[model] = atof(line.c_str());

      getline(ifstr, line);
      for (int dummy = 0; dummy < number_of_wavelength_points; dummy++) {
        getline(ifstr, line);
        std::string::size_type begin = line.find_first_of("dD");
        int idx = 0;
        while (begin < line.length()) {
          line.at(begin) = 'e';
          if (line.at(begin + 1) == ' ')
            line.at(begin + 1) = '+';
          mod[models]->dust[model][dummy][idx++] =
              atof((line.substr(begin - 5, 9)).c_str());
          begin = line.find_first_of("dD", begin + 1);
        }
      }
      ifstr >> line;
      line.at(5) = 'e';
      mod[models]->abundances[model] = atof(line.c_str());
      std::getline(ifstr, line);
    }

    ifstr >> line;
    mod[models]->parameters[IREADT][0] =
        strcpy(new char[strlen("IREADT") + 1], "IREADT");
    mod[models]->parameters[IREADT][1] =
        strcpy(new char[line.length() + 1], line.c_str());

    ifstr >> line;
    line.at(5) = 'e';
    mod[models]->parameters[TDS][0] =
        strcpy(new char[strlen("TDS") + 1], "TDS");
    mod[models]->parameters[TDS][1] =
        strcpy(new char[line.length() + 1], line.c_str());

    ifstr >> line;
    line.at(5) = 'e';
    mod[models]->parameters[TDC][0] =
        strcpy(new char[strlen("TDC") + 1], "TDC");
    mod[models]->parameters[TDC][1] =
        strcpy(new char[line.length() + 1], line.c_str());
    getline(ifstr, line);

    ifstr >> line;
    line.at(5) = 'e';
    mod[models]->parameters[RMAX][0] =
        strcpy(new char[strlen("RMAX") + 1], "RMAX");
    mod[models]->parameters[RMAX][1] =
        strcpy(new char[line.length() + 1], line.c_str());

    ifstr >> line;
    line.at(5) = 'e';
    mod[models]->parameters[TAU0F][0] =
        strcpy(new char[strlen("TAU0F") + 1], "TAU0f");
    mod[models]->parameters[TAU0F][1] =
        strcpy(new char[line.length() + 1], line.c_str());

    ifstr >> line;
    line.at(5) = 'e';
    mod[models]->parameters[RHOCS][0] =
        strcpy(new char[strlen("RHOCS") + 1], "RHOCS");
    mod[models]->parameters[RHOCS][1] =
        strcpy(new char[line.length() + 1], line.c_str());

    ifstr >> line;
    line.at(5) = 'e';
    mod[models]->parameters[TSTAR][0] =
        strcpy(new char[strlen("TSTAR") + 1], "TSTAR");
    mod[models]->parameters[TSTAR][1] =
        strcpy(new char[line.length() + 1], line.c_str());

    ifstr >> line;
    line.at(5) = 'e';
    mod[models]->parameters[TLUM][0] =
        strcpy(new char[strlen("TLUM") + 1], "TLUM");
    mod[models]->parameters[TLUM][1] =
        strcpy(new char[line.length() + 1], line.c_str());

    getline(ifstr, line, '=');
    mod[models]->parameters[ICONV][0] =
        strcpy(new char[line.length() + 1], line.c_str());
    ifstr >> line;
    mod[models]->parameters[ICONV][1] =
        strcpy(new char[line.length() + 1], line.c_str());

    if (atoi(line.c_str())) {
      getline(ifstr, line);
      index = 0;
      for (int dummy = 0; dummy < number_of_lines; dummy++) {
        std::getline(ifstr, line);

        std::string::size_type begin = line.find_first_of("dD");
        while (begin < line.length()) {
          line.at(begin) = 'e';
          if (line.at(begin + 1) == ' ')
            line.at(begin + 1) = '+';
          mod[models]->convolution[index++] =
              atof((line.substr(begin - 5, 9)).c_str());
          begin = line.find_first_of("dD", begin + 1);
        }
      }
    }
  }
  return (0);
}
