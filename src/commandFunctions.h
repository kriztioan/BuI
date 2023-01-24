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

int doexit(std::queue<std::string> &parameter);
int list_commands(std::queue<std::string> &parameter);
int help(std::queue<std::string> &parameter);
int save(std::queue<std::string> &parameter);
int display(std::queue<std::string> &parameter);
int writeplot(std::queue<std::string> &parameter);
int info(std::queue<std::string> &parameter);
int contour(std::queue<std::string> &parameter);
int return_func(std::queue<std::string> &parameter);
int unknown(std::queue<std::string> &parameter);
int show(std::queue<std::string> &parameter);
int create(std::queue<std::string> &parameter);
int shell(std::queue<std::string> &parameter);
int window(std::queue<std::string> &parameter);
int graph(std::queue<std::string> &parameter);

int set(std::queue<std::string> &parameter);
int run(std::queue<std::string> &parameter);

int read(std::ifstream &ifstr, float array[][100], int Nrows, int Ncol,
          char *delimeter);
int limits(float *array, int size, float &min, float &max);
int strToLower(std::string &toUpper);

int print(float Xarray[], float Yarray[], int size, bool file = false);
int plot(float Xarray[], float Yarray[], int size, char *Xlabel, char *Ylabel,
          char *toplabel, int color = 1, bool file = false);
int createOutput(device destination, std::queue<std::string> &parameter);

int setLevels(float array[][10], int nRows, int nCollumns, float levels[],
               float fLevels, float &min, float &max);
int toLog(float array[]);

int createRadialGrid();
int createWavelengthGrid();
int createDust();
int createConv();
int createIrf();

#endif // End of COMMANDFUNCTIONS_H_
