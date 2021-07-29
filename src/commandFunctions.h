/**
 *  @file   commandFunctions.h
 *  @brief  Command Function Definitions
 *  @author KrizTioaN (christiaanboersma@hotmail.com)
 *  @date   2021-07-29
 *  @note   BSD-3 licensed
 *
 ***********************************************/

#ifndef COMMANDFUNCTIONS_H_
#define COMMANDFUNCTIONS_H_

typedef enum { BUI_PLOT, FILENAME, PLOT2FILE } device;

void doexit(std::queue<std::string> &parameter);
void list_commands(std::queue<std::string> &parameter);
void help(std::queue<std::string> &parameter);
void save(std::queue<std::string> &parameter);
void display(std::queue<std::string> &parameter);
void writeplot(std::queue<std::string> &parameter);
void info(std::queue<std::string> &parameter);
void contour(std::queue<std::string> &parameter);
void return_func(std::queue<std::string> &parameter);
void unknown(std::queue<std::string> &parameter);
void show(std::queue<std::string> &parameter);
void create(std::queue<std::string> &parameter);
void shell(std::queue<std::string> &parameter);
void window(std::queue<std::string> &parameter);
void graph(std::queue<std::string> &parameter);

void set(std::queue<std::string> &parameter);
void run(std::queue<std::string> &parameter);

void read(std::ifstream &ifstr, float array[][100], int Nrows, int Ncol,
          char *delimeter);
void limits(float *array, int size, float &min, float &max);
void strToLower(std::string &toUpper);

void print(float Xarray[], float Yarray[], int size, bool file = false);
void plot(float Xarray[], float Yarray[], int size, char *Xlabel, char *Ylabel,
          char *toplabel, int color = 1, bool file = false);
void createOutput(device destination, std::queue<std::string> &parameter);

void setLevels(float array[][10], int nRows, int nCollumns, float levels[],
               float fLevels, float &min, float &max);
void toLog(float array[]);

void createRadialGrid();
void createWavelengthGrid();
void createDust();
void createConv();
void createIrf();

#endif // End of COMMANDFUNCTIONS_H_
