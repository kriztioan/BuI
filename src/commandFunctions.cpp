/**
 *  @file   commandFunctions.cpp
 *  @brief  Command Function Implementations
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#include "header.h"
#include "human_readable.h"
#include <cstdio>
extern "C" {
#include <malloc.h>
#include <readline/history.h>
}

extern ENV env;
extern WIN win;
extern STYLE style;
extern IO io;
extern COM *com;
extern MES *mes;
extern MOD *mod;

void shell(std::queue<std::string> &parameter) {
  std::string command;
  if (!parameter.empty()) {
    while (parameter.size() != 0) {
      command = command + parameter.front() + " ";
      parameter.pop();
    }
    system(command.c_str());
  } else
    std::cout << "Give command" << std::endl;
}

void help(std::queue<std::string> &parameter) {
  if (!parameter.empty()) {
    const char *name = parameter.front().c_str();
    for (int i = 0; i < N_COMMANDS; i++) {
      if (strcmp(com[i].name, name) == 0) {
        std::ifstream ifstr(env.help_file, std::ios::in);
        ifstr.seekg(com[i].offset, std::ios::beg);
        std::cout <<  "\n                    Command " << com[i].name;
        char character = '\n';
        do {
          std::cout << character;
          ifstr.get(character);
        } while (character != '[' && ifstr.good());

        ifstr.close();
        return;
      }
    }
    std::cout << "No help available for " << name << std::endl;
    return;
  }

  std::ifstream ifstr(env.help_file, std::ios::in);
  ifstr.seekg(com[HELP].offset, std::ios::beg);
  char character = '\n';
  do {
    std::cout << character;
    ifstr.get(character);
  } while (character != '[' && ifstr.good());
  ifstr.close();
  list_commands(parameter);
}

void list_commands(std::queue<std::string> &parameter) {
  std::cout << "\n For a specific topic try typing HELP <command> where command "
               "is one of the following:\n\n";
  for (int i = 0; i < N_COMMANDS; i++)
    std::cout << "  " << std::setw(12) << std::setiosflags(std::ios::left)
              << com[i].name << com[i].description << '\n';
  std::cout << std::endl;
}

void doexit(std::queue<std::string> &parameter) {
  message(ONEXIT);

  message(ENDPGPLOT);
  cpgend();

  message(WRITEHISTORY);
  write_history(env.history_file);

  history_truncate_file(env.history_file, env.history_lines);

  struct mallinfo2 memory;
  memory = mallinfo2();

  std::cout << "Allocated memory: total " << human_readable(memory.uordblks)
            << " / free " << human_readable(memory.fordblks) << '\n';

  message(FREEMEMORY);
  delete[] io.input_file;
  delete[] io.output_file;
  delete[] io.plot_file;
  delete[] io.data_file;

  int models = atoi(mod[0].parameters[NMODEL][1]);
  for (int i = 0; i < models; i++) {

    for (int j = 0; j < atoi(mod[i].parameters[IMIX][1]); j++) {
      delete mod[i].dustname[j][0];
      delete mod[i].dustname[j][1];
    }
    for (int k = 1; k < N_PARAMETERS; k++) {
      delete mod[i].parameters[k][0];
      delete mod[i].parameters[k][1];
    }
  }
  delete mod;
  for (int i = 0; i < N_COMMANDS; i++) {
    delete com[i].name;
    delete com[i].description;
  }
  delete com;
  for (int i = 0; i < N_MESSAGES; i++)
    delete mes[i].text;
  delete mes;

  free(env.help_file);
  free(env.language_file);

  struct tm stm;
  time_t t_delta = time(NULL) - env.t_start;
  stm = *localtime(&t_delta);
  std::cout << "Total time used by processor " << std::setprecision(2)
            << std::setiosflags(std::ios::fixed)
            << float(clock()) / float(CLOCKS_PER_SEC) << " seconds\n"
            << "Total uptime " << stm.tm_hour - 1 << " hours " << stm.tm_min
            << " minutes " << stm.tm_sec << " seconds" << std::endl;
  exit(0);
}

void display(std::queue<std::string> &parameter) {
  createOutput(BUI_PLOT, parameter);
}

void create(std::queue<std::string> &parameter) {
  if (!parameter.empty()) {
    strToLower(parameter.front());
    if (parameter.front() == "radialgrid")
      createRadialGrid();
    else if (parameter.front() == "wavelengthgrid")
      createWavelengthGrid();
    else if (parameter.front() == "irf")
      createIrf();
    else if (parameter.front() == "conv")
      createConv();
    else if (parameter.front() == "dust")
      createDust();
    else if (parameter.front() == "all") {
      createRadialGrid();
      createWavelengthGrid();
      createIrf();
      createDust();
      createConv();
    } else
      std::cout << "Give parameter RADIALGRID WAVELENGTHGRID IRF CONV DUST ALL"
                << std::endl;
  } else
    std::cout << "Give parameter RADIALGRID WAVELENGTHGRID IRF CONV DUST ALL"
              << std::endl;
}

void show(std::queue<std::string> &parameter) {
  if (!parameter.empty()) {
    std::cout.setf(std::ios::scientific);
    std::cout.precision(3);
    strToLower(parameter.front());
    if (parameter.front() == "spatialgrid") {
      std::cout << "Spatial Grid:\n\n";
      for (int idx = 1; idx < 101; idx++) {
        std::cout << mod[0].spatialgrid[idx - 1] << "\t";
        if (idx % 5 == 0)
          std::cout << '\n';
      }
      std::cout << std::endl;
    } else if (parameter.front() == "wavelengthgrid") {
      std::cout << "Wavelength Grid:\n\n";
      for (int idx = 1; idx < atoi(mod[0].parameters[NFD][1]) + 1; idx++) {
        std::cout << mod[0].wavelengthgrid[idx - 1] << "\t";
        if (idx % 5 == 0)
          std::cout << std::endl;
      }
      std::cout << std::endl;
    } else if (parameter.front() == "irf") {
      std::cout << "Interstellar Radiation Field:\n\n";
      for (int idx = 1; idx < atoi(mod[0].parameters[NFD][1]) + 1; idx++) {
        std::cout << mod[0].interstellar[idx - 1] << "\t";
        if (idx % 5 == 0)
          std::cout << std::endl;
      }
      std::cout << std::endl;
    } else if (parameter.front() == "conv") {
      std::cout << "Convolution Grid\n\n";
      for (int idx = 1; idx < atoi(mod[0].parameters[NFD][1]) + 1; idx++) {
        std::cout << mod[0].convolution[idx - 1] << "\t";
        if (idx % 5 == 0)
          std::cout << std::endl;
      }
      std::cout << std::endl;
    } else if (parameter.front() == "dust") {
      std::cout << "Dust Model Parameters\n\n";
      for (int idx = 0; idx < atoi(mod[0].parameters[IMIX][1]); idx++) {
        if (atoi(mod[0].parameters[IMIX][1]) > 1)
          std::cout << "Model " << idx + 1 << "\n\n";

        std::cout << "Grain Core Composition " << mod[0].dustname[idx][0]
                  << " radius " << mod[0].radii[idx][0]
                  << "\nGrain Mantle Compostion " << mod[0].dustname[idx][1]
                  << " radius " << mod[0].radii[idx][1] << "\nAbundance "
                  << mod[0].abundances[idx] << " Abundance Type "
                  << mod[0].type[idx] << std::endl
                  << std::endl;

        for (int index = 1; index < atoi(mod[0].parameters[NFD][1]) + 1;
             index++) {
          std::cout << mod[0].dust[idx][index - 1][0] << "\t"
                    << mod[0].dust[idx][index - 1][1] << "\t"
                    << mod[0].dust[idx][index - 1][2] << "\t"
                    << mod[0].dust[idx][index - 1][3] << std::endl
                    << std::endl;
        }
        if (idx % 5 == 0)
          std::cout << std::endl;
      }
    } else if (parameter.front() == "parameters") {
      for (int i = 0; i < atoi(mod[0].parameters[NMODEL][1]); i++) {
        if (atoi(mod[0].parameters[NMODEL][1]) > 1)
          std::cout << "Model " << i << "/" << mod[0].parameters[NMODEL][1]
                    << std::endl
                    << std::endl;
        std::cout
            << "Model Parameters:" << std::endl
            << std::endl
            << std::setw(55) << std::setiosflags(std::ios::left) << "Geometry "
            << (atoi(mod[i].parameters[IGEOM][1]) == 2
                    ? "spherical"
                    : "plane parallel or cylindrical")
            << std::endl
            << std::setw(55) << std::setiosflags(std::ios::left)
            << "Scattering is treated "
            << (std::atoi(mod[i].parameters[ISCA][1]) == 0 ? "isotropic"
                                                           : "anisotropic")
            << std::endl
            << std::setw(55) << std::setiosflags(std::ios::left)
            << "Number of angles for the cylindrical case "
            << (atoi(mod[i].parameters[IEMRG][1]) == 0 ? "4" : "1") << std::endl
            << std::setw(55) << std::setiosflags(std::ios::left)
            << "Eddington approximation "
            << (atoi(mod[i].parameters[IEDFTR][1]) == 0 ? "used" : "not used")
            << std::endl
            << std::setw(55) << std::setiosflags(std::ios::left)
            << "The normalization constant is set to "
            << mod[i].parameters[AJ0][1] << std::endl
            << std::setw(55) << std::setiosflags(std::ios::left)
            << "The maximuum number of iterations is set to "
            << mod[i].parameters[ITMAX][1] << std::endl
            << std::setw(55) << std::setiosflags(std::ios::left)
            << "The allowed error for convergence is set to "
            << mod[i].parameters[EPS][1] << std::endl
            << std::setw(55) << std::setiosflags(std::ios::left)
            << "Number of wavelength points used " << mod[i].parameters[NFD][1]
            << std::endl
            << std::setw(55) << std::setiosflags(std::ios::left)
            << "Number of grain types used " << mod[i].parameters[IMIX][1]
            << std::endl
            << std::endl
            << std::setw(55) << std::setiosflags(std::ios::left)
            << "Interstellar radiation field "
            << (atoi(mod[i].parameters[IB][1]) == 0 ? "not present" : "present")
            << std::endl
            << std::endl
            << std::setw(55) << std::setiosflags(std::ios::left)
            << "Central heat source "
            << (atoi(mod[i].parameters[IC][1]) == 0 ? "not present" : "present")
            << std::endl
            << std::setw(55) << std::setiosflags(std::ios::left)
            << "Effective temperature central heat source (K) "
            << mod[i].parameters[TSTAR][1] << std::endl
            << std::setw(55) << std::setiosflags(std::ios::left)
            << "Effective luminosity central heat source (K) "
            << mod[i].parameters[TLUM][1] << std::endl
            << std::setw(55) << std::setiosflags(std::ios::left)
            << "Flux from central heat source is treated "
            << (atoi(mod[i].parameters[NH][1]) == 0 ? "as incident flux"
                                                    : "as net flux")
            << std::endl
            << std::endl
            << std::setw(55) << std::setiosflags(std::ios::left)
            << "Optical depth reference wavelength (micron) "
            << mod[i].wavelengthgrid[atoi(mod[i].parameters[IOF][1]) - 1]
            << std::endl
            << std::setw(55) << std::setiosflags(std::ios::left)
            << "Optical depth at reference wavelength is "
            << mod[i].parameters[TAU0F][1] << std::endl
            << std::endl
            << std::setw(55) << std::setiosflags(std::ios::left)
            << "The outer cloud radius is set to (pc) "
            << mod[i].parameters[RMAX][1] << std::endl
            << std::setw(55) << std::setiosflags(std::ios::left)
            << "The dust distribution "
            << (atoi(mod[i].parameters[IDIST][1]) == 0 ? "power law"
                                                       : "constant")
            << std::endl
            << std::setw(55) << std::setiosflags(std::ios::left)
            << "Power law index to the dust distribution is "
            << mod[i].parameters[RHOCS][1] << std::endl
            << std::setw(55) << std::setiosflags(std::ios::left)
            << "Initial guess dust temperature is "
            << (atoi(mod[i].parameters[IREADT][1]) == 0 ? "calculated"
                                                        : "read in")
            << std::endl
            << std::setw(55) << std::setiosflags(std::ios::left)
            << "Estimated dust temperture at outer boundary (K)"
            << mod[i].parameters[TDS][1] << std::endl
            << std::setw(55) << std::setiosflags(std::ios::left)
            << "Estimated dust temperture at inner boundary (K)"
            << mod[i].parameters[TDC][1] << std::endl
            << std::endl
            << std::setw(55) << std::setiosflags(std::ios::left) << "Output is "
            << (atoi(mod[i].parameters[ICONV][1]) == 0 ? "not convolved"
                                                       : "covolved")
            << std::endl
            << std::setw(55) << std::setiosflags(std::ios::left) << "Output is "
            << (atoi(mod[i].parameters[IOUT][1]) == 0 ? "simple" : "detailed")
            << std::endl;
      }
    } else
      std::cout << "Give argument: SPATIALGRID WAVELENGTHGRID IRF CONV DUST "
                   "PARAMETERS";
  } else
    std::cout
        << "Give argument: SPATIALGRID WAVELENGTHGRID IRF CONV DUST PARAMETERS";
  std::cout << std::endl;
}

void info(std::queue<std::string> &parameter) {
  std::cout << "Info\n"
            << std::endl
            << "Inputfile\t\t" << io.input_file << std::endl
            << "Outputfile\t\t" << io.output_file << std::endl
            << "Plotfile\t\t" << io.plot_file << std::endl
            << "Datafile\t\t" << io.data_file << std::endl
            << std::endl;
}

void save(std::queue<std::string> &parameter) {
  createOutput(FILENAME, parameter);
}

void return_func(std::queue<std::string> &parameter) {}

void unknown(std::queue<std::string> &parameter) {
  std::cout << "Unknown command" << std::endl;
}

void run(std::queue<std::string> &parameter) {
  std::string command;
  if (getenv("CSDUST3") == NULL) {
    std::cout << "CSDUST3 enviornment variable not set" << std::endl;
    return;
  } else
    command = getenv("CSDUST3");

  command = command + " < " + io.input_file + " > " + io.output_file;

  std::cout << "Reading from: " << io.input_file
            << "\nWritting to: " << io.output_file << "\nRunning CSDUST3 ..."
            << std::endl;

  system(command.c_str());

  /*std::procbuf::procbuf pb;
  pb.open(command.c_str(), std::ios::in);
  std::istream ipb(&pb);

  std::ofstream ofstr(io.output_file, std::ios::out);

  while (ipb.good()) {
    // std::getline(ipb, command);

    std::string::size_type loc = command.find("9100");
    if ((loc != std::string::npos) && (loc == 0))
      command.insert(1, " ");

    ofstr << command << std::endl;
  }
  pb.close();
  ofstr.close();

  std::ifstream ifstr(io.output_file, std::ios::in);
  std::string dummy;
  while (ifstr.good()) {
    std::getline(ifstr, dummy);
    if ((dummy.find("CONVERGENCE") != std::string::npos) &&
        (dummy.find("PARAMETER") == std::string::npos)) {
      dummy.erase(dummy.find_first_of("\t "),
                  dummy.find_first_not_of("\t ") - dummy.find_first_of("\t "));
      strToLower(dummy);
      dummy[0] = toupper(dummy[0]);
      std::cout << dummy << std::endl;
      break;
    }
  }
  if (ifstr.eof())
    std::cout
        << "Error while running model, check outputfile for further details"
        << std::endl;*/
  std::cout << "completed" << std::endl;
}

void set(std::queue<std::string> &parameter) {
  if (parameter.size() > 1) {
    strToLower(parameter.front());
    if (parameter.front() == "output") {
      parameter.pop();
      delete io.output_file;
      io.output_file = strcpy(new char[parameter.front().size() + 1],
                              parameter.front().c_str());
    } else if (parameter.front() == "input") {
      parameter.pop();
      delete io.input_file;
      io.input_file = strcpy(new char[parameter.front().size() + 1],
                             parameter.front().c_str());
      loadInputfile(io.input_file, &mod);
    } else if (parameter.front() == "plot") {
      parameter.pop();
      delete io.plot_file;
      io.plot_file = strcpy(new char[parameter.front().size() + 1],
                            parameter.front().c_str());
    } else if (parameter.front() == "data") {
      parameter.pop();
      delete io.data_file;
      io.data_file = strcpy(new char[parameter.front().size() + 1],
                            parameter.front().c_str());
    } else
      std::cout << "Give argument: INPUT OUTPUT PLOT" << std::endl;
  } else
    std::cout << "Give argument: INPUT OUTPUT PLOT DATA [filename]"
              << std::endl;
}

void writeplot(std::queue<std::string> &parameter) {
  createOutput(PLOT2FILE, parameter);
}

void contour(std::queue<std::string> &parameter) {

  std::ifstream iFile(io.output_file, std::ios::in);
  if (iFile.good()) {

    int arraySize;
    std::string dummy;
    while (std::getline(iFile, dummy)) {
      if (dummy.find("NUMBER OF FREQUENCY POINTS, NFD") != std::string::npos) {
        std::string::size_type pos = dummy.find_last_of("=") + 1;
        arraySize = static_cast<int>(strtod(
            dummy.substr(dummy.find_first_not_of(" ", pos)).c_str(), NULL));
        break;
      }
    }
    std::cout << "Contouring for " << arraySize << " frequency points"
              << std::endl;

    // Find and fill data array

    float frequencies[arraySize], grid[arraySize][10][10];

    float dummyFloat;
    while (std::getline(iFile, dummy)) {
      if (dummy.find("1 IF") != std::string::npos) {
        for (int idx = 0; idx < arraySize; idx++) {
          iFile >> frequencies[idx];
          iFile >> frequencies[idx];
          for (int skip = 0; skip < 4; skip++)
            iFile >> dummyFloat;
        }
        break;
      }
    }
    int x = -1;
    while (std::getline(iFile, dummy)) {
      if (dummy.find("INTENSITIES, AT GIVEN X AND Y  FROM CLOUD CENTER") !=
          std::string::npos) {
        x++;
        std::getline(iFile, dummy);
        std::getline(iFile, dummy);
        for (int z = 0; z < arraySize; z++) {
          for (int y = 0; y < 4; y++)
            iFile >> grid[z][y][x];
          for (int y = 0; y < 10; y++)
            iFile >> grid[z][y][x];
        }
        if (x > 9)
          break;
      }
    }
    iFile.close();

    // setting up levels for contours

    int nLevels = 49;
    float fLevels = nLevels;
    float levels[nLevels], TR[6] = {0, 1, 0, 0, 0, 1};

    // plotting countours

    cpgopen("/XWINDOW");
    cpgask(true);
    cpgpap(win.windowSize, 0.9);
    cpgscr(0, 1, 1, 1);
    cpgscr(1, 0, 0, 0);
    cpgsubp(2, 3);

    for (int plots = 0; plots < arraySize; plots++) {

      float min, max;
      setLevels(grid[plots], 10, 10, levels, fLevels, min, max);

      std::ostringstream ostrstr;
      ostrstr << "Countour map at " << frequencies[plots] << " micron"
              << std::ends;
      std::string label;
      label = ostrstr.str();

      cpgsci(1);
      cpgscf(2);
      cpgsch(1);
      cpgpage();
      cpgswin(1, 10, 1, 10);
      cpgsvp(0.1, 0.8, 0.1, 0.90);
      cpgsci(2);
      cpgslw(1);

      for (int color = 0; color < fLevels; color++) {
        float R, G, B = 0;
        R = pow(color / fLevels, 1.5);
        G = 1 - 0.25 * color / fLevels;
        B = 1 - color / fLevels;
        cpgscr(16 + color, R, G, B);
      }
      for (int fill = 0; fill < nLevels - 1; fill++) {
        cpgsci(fill + 16);
        cpgconf(grid[plots][0], 10, 10, 1, 10, 1, 10, levels[fill],
                levels[fill + 1], TR);
      }
      cpgsci(1);
      cpgswin(0, 9, 0, 9);
      cpgbox("BCNTST", 1, 0, "BCNTST", 1, 0);
      cpgswin(1, 10, 1, 10);
      cpgslw(3);
      cpglab("X position on cloud [r/(9 * R)]",
             "Y position on cloud [r/(9 * R)]", label.c_str());
      cpgslw(1);

      // Create legend
      cpgsvp(0.85, 0.95, 0.1, 0.90);
      cpgsls(1);
      for (int color = 0; color < nLevels; color++) {
        float width[2] = {1, 7}, height[2];
        height[0] = 5 * color / fLevels + 3;
        height[1] = 5 * (color + 1) / fLevels + 3;
        cpgsci(color + 16);
        cpgrect(width[0], width[1], height[0], height[1]);
      }

      // Write values to legend
      cpgsci(1);
      cpgsch(0.75);
      int sEnumerate = nLevels / 3;
      for (int enumerate = 0; enumerate < nLevels; enumerate += sEnumerate) {
        std::ostringstream ostrstrLab;
        ostrstrLab.precision(2);
        ostrstrLab << levels[enumerate] << std::ends;
        label = ostrstrLab.str();

        float x, y;
        x = 8;
        y = 5 * enumerate / fLevels + 3;
        cpgtext(x, y, label.c_str());
      }
    }
    cpgclos();
    cpgslct(win.graphicsID);
    cpgask(false);
  } else
    std::cout << "Unable to open output file" << std::endl;
}

void graph(std::queue<std::string> &parameter) {
  if (parameter.size() > 1) {
    strToLower(parameter.front());
    if (parameter.front() == "symbol") {
      parameter.pop();
      style.symbol = atoi(parameter.front().c_str());
    } else if (parameter.front() == "color") {
      parameter.pop();
      style.color = atoi(parameter.front().c_str());
    } else if (parameter.front() == "lineweight") {
      parameter.pop();
      style.lineweight = atoi(parameter.front().c_str());
    } else if (parameter.front() == "type") {
      parameter.pop();
      style.type = *parameter.front().c_str();
    } else
      std::cout << "Give parameter SYMBOL COLOR LINEWEIGHT TYPE <VALUE>"
                << std::endl;
  } else
    std::cout << "Give parameter SYMBOL COLOR LINEWEIGHT TYPE <VALUE>"
              << std::endl;
}

void window(std::queue<std::string> &parameter) {

  if (parameter.size() > 1) {
    strToLower(parameter.front());
    if (parameter.front() == "size") {
      parameter.pop();
      win.windowSize = atof(parameter.front().c_str());
      cpgpap(win.windowSize, 0.9);
      cpgpage();
    } else
      std::cout << "Give parameter SIZE <VALUE>" << std::endl;
  } else
    std::cout << "Give parameter SIZE <VALUE>" << std::endl;
}

// Auxillary Functions

void createOutput(device destination, std::queue<std::string> &parameter) {

  if (!parameter.empty()) {
    std::ifstream iFile(io.output_file, std::ios::in);
    if (iFile.good()) {
      std::string quantity = parameter.front();
      strToLower(quantity);
      parameter.pop();
      bool logX = false, logY = false;
      if (parameter.size() >= 1) {
        strToLower(parameter.front());
        if (parameter.front() == "logx") {
          logX = true;
          parameter.pop();
          if (parameter.size() >= 1) {
            strToLower(parameter.front());
            if (parameter.front() == "logy")
              logY = true;
          }
        } else if (parameter.front() == "logy")
          logY = true;
      }

      if (quantity == "spec") {
        int arraySize;
        std::string dummy;
        while (std::getline(iFile, dummy)) {
          if (dummy.find("NUMBER OF FREQUENCY POINTS, NFD") !=
              std::string::npos) {
            std::string::size_type pos = dummy.find_last_of("=") + 1;
            arraySize = static_cast<int>(strtod(
                dummy.substr(dummy.find_first_not_of(" ", pos)).c_str(), NULL));
            break;
          }
        }
        float dataField[14][arraySize];

        while ((dummy.find("CONVOLVED INTENSITIES, AT GIVEN X AND Y  FROM "
                           "CLOUD CENTER Y = 0.000") == std::string::npos) &&
               (iFile.good())) {
          std::getline(iFile, dummy);
        }
        std::getline(iFile, dummy);
        for (int row = 0; row < arraySize; row++) {
          for (int collum = 0; collum < 14; collum++) {
            iFile >> dataField[collum][row];
          }
        }

        if (logX)
          toLog(dataField[2]);
        if (logY)
          toLog(dataField[4]);

        switch (destination) {
        case BUI_PLOT:
          plot(dataField[2], dataField[4], arraySize, "Wavelength (\\gmm) ->",
               "Intensity (W . m-2 . Hz-1 . sr-1) ->",
               "Convolved Intensities at Cloud Center", 7);
          break;
        case FILENAME:
          print(dataField[2], dataField[4], arraySize, true);
          break;
        case PLOT2FILE:
          plot(dataField[2], dataField[4], arraySize, "Wavelength (\\gmm) ->",
               "Intensity (W . m-2 . Hz-1 . sr-1) ->",
               "Convolved Intensities at Cloud Center", 7, true);
        }
      } else if (quantity == "emerg") {
        int arraySize;
        std::string dummy;
        while (getline(iFile, dummy)) {
          if (dummy.find("NUMBER OF FREQUENCY POINTS, NFD") !=
              std::string::npos) {
            std::string::size_type pos = dummy.find_last_of("=") + 1;
            arraySize = static_cast<int>(strtod(
                dummy.substr(dummy.find_first_not_of(" ", pos)).c_str(), NULL));
            break;
          }
        }
        float dataField[14][arraySize];

        while ((dummy.find("EMERGENT INTENSITIES AND FLUXES") ==
                std::string::npos) &&
               (iFile.good())) {
          std::getline(iFile, dummy);
        }
        std::getline(iFile, dummy);
        for (int row = 0; row < arraySize; row++) {
          for (int collum = 0; collum < 12; collum++)
            iFile >> dataField[collum][row];
        }

        if (logX)
          toLog(dataField[1]);
        if (logY)
          toLog(dataField[11]);

        switch (destination) {
        case BUI_PLOT:
          plot(dataField[1], dataField[11], arraySize, "Wavelength (\\gmm) ->",
               "Flux (W . m-2 . Hz-1) ->", "Emergent Flux", 6);
          break;
        case FILENAME:
          print(dataField[1], dataField[11], arraySize, true);
          break;
        case PLOT2FILE:
          plot(dataField[2], dataField[4], arraySize, "Wavelength (\\gmm) ->",
               "Flux (W . m-2 . Hz-1) ->", "Emergent Flux", 6, true);
        }
      } else if (quantity == "rhod") {
        float dataField[9][100];
        read(iFile, dataField, 100, 9,
             "1 IR       R        RHOD      TAUOF       TD       AVFLUX      "
             "COOLD      HEATD");

        if (logX)
          toLog(dataField[2]);
        if (logY)
          toLog(dataField[3]);

        switch (destination) {
        case BUI_PLOT:
          plot(dataField[2], dataField[3], 100, "r/R ->", "RHOD ->",
               "Radial Dust Density Distribution", 3);
          break;
        case FILENAME:
          print(dataField[2], dataField[3], 100, true);
          break;
        case PLOT2FILE:
          plot(dataField[2], dataField[3], 100, "r/R ->", "RHOD ->",
               "Radial Dust Density Distribution", 3, true);
        }
      } else if (quantity == "tau0f") {
        float dataField[9][100];
        read(iFile, dataField, 100, 9,
             "1 IR       R        RHOD      TAUOF       TD       AVFLUX      "
             "COOLD      HEATD");

        if (logX)
          toLog(dataField[2]);
        if (logY)
          toLog(dataField[4]);

        switch (destination) {
        case BUI_PLOT:
          plot(dataField[2], dataField[4], 100, "r/R ->", "TAUOF ->",
               "Riadial Optical Depth Dependence at the Reference Frequentie",
               4);
          break;
        case FILENAME:
          print(dataField[2], dataField[4], 100, true);
          break;
        case PLOT2FILE:
          plot(dataField[2], dataField[4], 100, "r/R ->", "TAUOF ->",
               "Riadial Optical Depth Dependence at the Reference Frequentie",
               4, true);
        }
      } else if (quantity == "avflux") {
        float dataField[9][100];
        read(iFile, dataField, 100, 9,
             "1 IR       R        RHOD      TAUOF       TD       AVFLUX      "
             "COOLD      HEATD");

        if (logX)
          toLog(dataField[2]);
        if (logY)
          toLog(dataField[6]);

        switch (destination) {
        case BUI_PLOT:
          plot(dataField[2], dataField[6], 100, "r/R ->", "TAUOF ->",
               "Riadial Optical Depth Dependence at the Reference Frequentie",
               4);
          break;
        case FILENAME:
          print(dataField[2], dataField[6], 100, true);
          break;
        case PLOT2FILE:
          plot(dataField[2], dataField[6], 100, "r/R ->", "TAUOF ->",
               "Riadial Optical Depth Dependence at the Reference Frequentie",
               4, true);
        }
      } else if (quantity == "coold") {
        float dataField[9][100];
        read(iFile, dataField, 100, 9,
             "1 IR       R        RHOD      TAUOF       TD       AVFLUX      "
             "COOLD      HEATD");

        if (logX)
          toLog(dataField[2]);
        if (logY)
          toLog(dataField[7]);

        switch (destination) {
        case BUI_PLOT:
          plot(dataField[2], dataField[7], 100, "r/R ->", "COOLD ->",
               "Radial Dust Cooling Distribution", 7);
          break;
        case FILENAME:
          print(dataField[2], dataField[7], 100, true);
          break;
        case PLOT2FILE:
          plot(dataField[2], dataField[7], 100, "r/R ->", "COOLD ->",
               "Radial Dust Cooling Distribution", 7, true);
        }
      } else if (quantity == "heatd") {
        float dataField[9][100];
        read(iFile, dataField, 100, 9,
             "1 IR       R        RHOD      TAUOF       TD       AVFLUX      "
             "COOLD      HEATD");

        if (logX)
          toLog(dataField[2]);
        if (logY)
          toLog(dataField[8]);

        switch (destination) {
        case BUI_PLOT:
          plot(dataField[2], dataField[8], 100, "r/R ->", "HEATD ->",
               "Radial Dust Heating Distribution", 8);
          break;
        case FILENAME:
          print(dataField[2], dataField[8], 100, true);
          break;
        case PLOT2FILE:
          plot(dataField[2], dataField[8], 100, "r/R ->", "HEATD ->",
               "Radial Dust Heating Distribution", 8, true);
        }
      } else if (quantity == "td") {
        float dataField[9][100];
        read(iFile, dataField, 100, 9,
             "1 IR       R        RHOD      TAUOF       TD       AVFLUX      "
             "COOLD      HEATD");

        if (logX)
          toLog(dataField[2]);
        if (logY)
          toLog(dataField[5]);

        switch (destination) {
        case BUI_PLOT:
          plot(dataField[2], dataField[5], 100, "r/R ->", "TD ->",
               "Radial Dust Temperature Distribution", 5);
          break;
        case FILENAME:
          print(dataField[2], dataField[5], 100, true);
          break;
        case PLOT2FILE:
          plot(dataField[2], dataField[5], 100, "r/R ->", "TD ->",
               "Radial Dust Temperature Distribution", 5, true);
        }
      } else if (quantity == "abundi1") {
        float dataField[11][100];
        read(iFile, dataField, 100, 11,
             "1 IR      R      ABUNDI( 1)  TDI( 1)   COOLDI( 1) HEATDI( 1) "
             "ABUNDI( 2)  TDI( 2)   COOLDI( 2) HEATDI( 2)");

        if (logX)
          toLog(dataField[2]);
        if (logY)
          toLog(dataField[3]);

        switch (destination) {
        case BUI_PLOT:
          plot(dataField[2], dataField[3], 100, "r/R ->", "ABUNDI( 1) - >",
               "Toplabel", 9);
          break;
        case FILENAME:
          print(dataField[2], dataField[3], 100, true);
          break;
        case PLOT2FILE:
          plot(dataField[2], dataField[3], 100, "r/R ->", "ABUNDI( 1) - >",
               "Toplabel", 9, true);
        }
      } else if (quantity == "tdi1") {
        float dataField[11][100];
        read(iFile, dataField, 100, 11,
             "1 IR      R      ABUNDI( 1)  TDI( 1)   COOLDI( 1) HEATDI( 1) "
             "ABUNDI( 2)  TDI( 2)   COOLDI( 2) HEATDI( 2)");

        if (logX)
          toLog(dataField[2]);
        if (logY)
          toLog(dataField[4]);

        switch (destination) {
        case BUI_PLOT:
          plot(dataField[2], dataField[4], 100, "r/R ->", "TDI( 1) ->",
               "Toplabel", 10);
          break;
        case FILENAME:
          print(dataField[2], dataField[4], 100, true);
          break;
        case PLOT2FILE:
          plot(dataField[2], dataField[4], 100, "r/R ->", "TDI( 1) ->",
               "Toplabel", 10, true);
        }
      } else if (quantity == "cooldi1") {
        float dataField[11][100];
        read(iFile, dataField, 100, 11,
             "1 IR      R      ABUNDI( 1)  TDI( 1)   COOLDI( 1) HEATDI( 1) "
             "ABUNDI( 2)  TDI( 2)   COOLDI( 2) HEATDI( 2)");

        if (logX)
          toLog(dataField[2]);
        if (logY)
          toLog(dataField[5]);

        switch (destination) {
        case BUI_PLOT:
          plot(dataField[2], dataField[5], 100, "r/R ->", "COOLDI( 1) ->",
               "Toplabel", 11);
          break;
        case FILENAME:
          plot(dataField[2], dataField[5], 100, "r/R ->", "COOLDI( 1) ->",
               "Toplabel", 11, true);
          break;
        case PLOT2FILE:
          plot(dataField[2], dataField[5], 100, "r/R ->", "COOLDI( 1) ->",
               "Toplabel", 11, true);
        }
      } else if (quantity == "heatdi1") {
        float dataField[11][100];
        read(iFile, dataField, 100, 11,
             "1 IR      R      ABUNDI( 1)  TDI( 1)   COOLDI( 1) HEATDI( 1) "
             "ABUNDI( 2)  TDI( 2)   COOLDI( 2) HEATDI( 2)");

        if (logX)
          toLog(dataField[2]);
        if (logY)
          toLog(dataField[6]);

        switch (destination) {
        case BUI_PLOT:
          plot(dataField[2], dataField[6], 100, "r/R ->", "HEATDI( 1) ->",
               "Toplabel", 12);
          break;
        case FILENAME:
          print(dataField[2], dataField[6], 100, true);
          break;
        case PLOT2FILE:
          plot(dataField[2], dataField[6], 100, "r/R ->", "HEATDI( 1) ->",
               "Toplabel", 12, true);
        }
      } else if (quantity == "abundi2") {
        float dataField[11][100];
        read(iFile, dataField, 100, 11,
             "1 IR      R      ABUNDI( 1)  TDI( 1)   COOLDI( 1) HEATDI( 1) "
             "ABUNDI( 2)  TDI( 2)   COOLDI( 2) HEATDI( 2)");

        if (logX)
          toLog(dataField[2]);
        if (logY)
          toLog(dataField[7]);

        switch (destination) {
        case BUI_PLOT:
          plot(dataField[2], dataField[7], 100, "r/R ->", "ABUNDI( 2) ->",
               "Toplabel", 13);
          break;
        case FILENAME:
          print(dataField[2], dataField[7], 100, true);
          break;
        case PLOT2FILE:
          plot(dataField[2], dataField[7], 100, "r/R ->", "ABUNDI( 2) ->",
               "Toplabel", 13, true);
        }
      } else if (quantity == "tdi2") {
        float dataField[11][100];
        read(iFile, dataField, 100, 11,
             "1 IR      R      ABUNDI( 1)  TDI( 1)   COOLDI( 1) HEATDI( 1) "
             "ABUNDI( 2)  TDI( 2)   COOLDI( 2) HEATDI( 2)");

        if (logX)
          toLog(dataField[2]);
        if (logY)
          toLog(dataField[8]);

        switch (destination) {
        case BUI_PLOT:
          plot(dataField[2], dataField[8], 100, "r/R ->", "TDI( 2) ->",
               "Toplabel", 11);
          break;
        case FILENAME:
          print(dataField[2], dataField[8], 100, true);
          break;
        case PLOT2FILE:
          plot(dataField[2], dataField[8], 100, "r/R ->", "TDI( 2) ->",
               "Toplabel", 11, true);
        }
      } else if (quantity == "cooldi2") {
        float dataField[11][100];
        read(iFile, dataField, 100, 11,
             "1 IR      R      ABUNDI( 1)  TDI( 1)   COOLDI( 1) HEATDI( 1) "
             "ABUNDI( 2)  TDI( 2)   COOLDI( 2) HEATDI( 2)");

        if (logX)
          toLog(dataField[2]);
        if (logY)
          toLog(dataField[9]);

        switch (destination) {
        case BUI_PLOT:
          plot(dataField[2], dataField[9], 100, "r/R ->", "COOLDI( 2) ->",
               "Toplabel", 14);
          break;
        case FILENAME:
          print(dataField[2], dataField[9], 100, true);
          break;
        case PLOT2FILE:
          plot(dataField[2], dataField[9], 100, "r/R ->", "COOLDI( 2) ->",
               "Toplabel", 14, true);
        }
      } else if (quantity == "heatdi2") {
        float dataField[11][100];
        read(iFile, dataField, 100, 11,
             "1 IR      R      ABUNDI( 1)  TDI( 1)   COOLDI( 1) HEATDI( 1) "
             "ABUNDI( 2)  TDI( 2)   COOLDI( 2) HEATDI( 2)");

        if (logX)
          toLog(dataField[2]);
        if (logY)
          toLog(dataField[10]);

        switch (destination) {
        case BUI_PLOT:
          plot(dataField[2], dataField[10], 100, "r/R ->", "HEATDI( 2)->",
               "Toplabel", 13);
          break;
        case FILENAME:
          print(dataField[2], dataField[10], 100, true);
          break;
        case PLOT2FILE:
          plot(dataField[2], dataField[10], 100, "r/R ->", "HEATDI( 2)->",
               "Toplabel", 13, true);
        }
      } else if (quantity == "irf") {

        if (logX) {
          for (int i = 0; i < atoi(mod[0].parameters[NFD][1]); i++)
            mod[0].wavelengthgrid[i] = log10(mod[0].wavelengthgrid[i]);
        }
        if (logY) {
          for (int i = 0; i < atoi(mod[0].parameters[NFD][1]); i++)
            mod[0].interstellar[i] = log10(mod[0].interstellar[i]);
        }

        switch (destination) {
        case BUI_PLOT:
          plot(mod[0].wavelengthgrid, mod[0].interstellar,
               atoi(mod[0].parameters[NFD][1]), "Wavelength (micron) ->",
               "Flux ->", "Interstellar radiation field", 4);
          break;
        case FILENAME:
          print(mod[0].wavelengthgrid, mod[0].interstellar,
                atoi(mod[0].parameters[NFD][1]), true);
          break;
        case PLOT2FILE:
          plot(mod[0].wavelengthgrid, mod[0].interstellar,
               atoi(mod[0].parameters[NFD][1]), "Wavelength (micron) ->",
               "Flux ->", "Interstellar radiation field", 4, true);
        }

        if (logX) {
          for (int i = 0; i < atoi(mod[0].parameters[NFD][1]); i++)
            mod[0].wavelengthgrid[i] = pow(10, mod[0].wavelengthgrid[i]);
        }
        if (logY) {
          for (int i = 0; i < atoi(mod[0].parameters[NFD][1]); i++)
            mod[0].interstellar[i] = pow(10, mod[0].interstellar[i]);
        }

      } else {
        std::cout << "Give argument: IRF SPEC EMERG RHOD TAU0F AVFLUX COOLD "
                     "HEATD TD ABUNDI# TDI# COOLDI# HEATDI# (LOGX / LOGY)"
                  << std::endl;
      }
    } else {
      std::cout << "Outputfile does not exist" << std::endl;
    }
  } else {
    std::cout << "Give argument: IRF SPEC EMERG RHOD TAU0F AVFLUX COOLD HEATD "
                 "TD ABUNDI# TDI# COOLDI# HEATD# (LOGX / LOGY)"
              << std::endl;
  }
}

void read(std::ifstream &ifstr, float array[][100], int Nrow, int Ncol,
          char *delimeter) {
  std::string dummy;
  while ((dummy.find(delimeter) == std::string::npos) && (ifstr.good())) {
    std::getline(ifstr, dummy);
  }

  for (int row = 0; row < Nrow; row++) {
    for (int collum = 0; collum < Ncol; collum++) {
      ifstr >> array[collum][row];
    }
  }
}

void print(float Xarray[], float Yarray[], int size, bool file) {

  if (file) {
    std::cout << "Writting to " << io.data_file << std::endl;
    std::ofstream ofstr(io.data_file, std::ios::out);
    ofstr.setf(std::ios::scientific);
    ofstr.precision(3);

    for (int idx = 0; idx < size; idx++)
      ofstr << Xarray[idx] << "\t" << Yarray[idx] << std::endl;
  } else {
    for (int idx = 0; idx < size; idx++) {
      std::cout << Xarray[idx] << "\t" << Yarray[idx] << std::endl;
    }
  }
}

void plot(float Xarray[], float Yarray[], int size, char *Xlabel, char *Ylabel,
          char *toplabel, int color, bool file) {

  ccdraw mydraw;
  ccplot myplot;
  cccurve mycurve;

  mycurve.x(size, Xarray);
  mycurve.y(size, Yarray);
  mycurve.color(style.color);
  mycurve.symbol(style.symbol);
  mycurve.lweight(style.lineweight);
  mycurve.type(style.type);

  if (file)
    std::cout << "Press 's' to save to file" << std::endl;

  myplot.add(mycurve);
  myplot.title(toplabel);
  myplot.xlabel(Xlabel);
  myplot.ylabel(Ylabel);

  mydraw.open(win.graphicsID);
  mydraw.window(win.windowSize, 0.9 * win.windowSize);
  mydraw.retain();
  mydraw.add(myplot);
  mydraw.paint();
}

void limits(float *array, int size, float &min, float &max) {
  min = array[0];
  max = array[0];

  int idx = 1;
  while (idx < size) {
    if (array[idx] < min)
      min = array[idx];
    if (array[idx] > max)
      max = array[idx];
    idx++;
  }

  if (min == max) {
    min = min - min / 20;
    max = max + max / 20;
  }

  min = min - ((max - min) / 20);
  max = max + ((max - min) / 20);
}

void strToLower(std::string &toUpper) {
  for (int pos = 0; pos < toUpper.size(); pos++)
    toUpper.at(pos) = tolower(toUpper.c_str()[pos]);
}

void setLevels(float array[][10], int nRows, int nCollumns, float levels[],
               float fLevels, float &min, float &max) {
  min = array[0][0];
  max = array[0][0];

  for (int r = 0; r < nRows; r++) {
    for (int c = 0; c < nCollumns; c++) {
      if (array[r][c] < min)
        min = array[r][c];
      if (array[r][c] > max)
        max = array[r][c];
    }
  }

  max = max * 1.1;
  if (min > 0)
    min = min * 0.9;
  else
    min = min * 1.1;
  for (int idx = 0; idx < fLevels; idx++)
    levels[idx] = (min + idx * (max - min) / fLevels);
}

void toLog(float array[]) {
  for (int idx = 0; idx < 100; idx++)
    array[idx] = log10(array[idx]);
}

void createRadialGrid() { std::cout << "Not implemented" << std::endl; }

void createWavelengthGrid() { std::cout << "Not implemented" << std::endl; }

void createConv() { std::cout << "Not implemented" << std::endl; }

void createIrf() { std::cout << "Not implemented" << std::endl; }

void createDust() { std::cout << "Not implemented" << std::endl; }
