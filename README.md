
# BuI

## Description

`BuI` is a command line user interface to [CSDUST3](https://mendeley.figshare.com/articles/dataset/CSDUST3_A_radiation_transport_code_for_a_dusty_medium_with_1-d_planar_spherical_or_cylindrical_geometry/11330666/1). It is written in `C/C++`. `BuI` offers an interactive terminal environment for constructing the input, run, and evaluating the output of [CSDUST3](https://mendeley.figshare.com/articles/dataset/CSDUST3_A_radiation_transport_code_for_a_dusty_medium_with_1-d_planar_spherical_or_cylindrical_geometry/11330666/1). It does this by offering simple commands that are able to generate the necessary input, running [CSDUST3](https://mendeley.figshare.com/articles/dataset/CSDUST3_A_radiation_transport_code_for_a_dusty_medium_with_1-d_planar_spherical_or_cylindrical_geometry/11330666/1) from within `BuI`, and generating figures from its output using the [CCPGPLOT](https://github.com/kriztioan/CCPGPLOT) `C++`plotting library. `BuI` has some rudimentary support for scripting.

A research report that describes and makes use of `BuI` can be found in the  [docs](docs/).

## Usage

`BuI` has at least the following requirements:

1. `GNU` `C++` compiler
2. `GNU` `Fortran` compiler
3. `GNU` `readline`/`history` libraries
4. `PGPlot`
5. `X11`

The terminal application is build with:

```shell
make
```

This results in a binary executable called `bui`, which is run as:

```shell
bui
```

From within `BuI`, running the `help` command is the place to start.

A modified copy of the [CSDUST3](https://mendeley.figshare.com/articles/dataset/CSDUST3_A_radiation_transport_code_for_a_dusty_medium_with_1-d_planar_spherical_or_cylindrical_geometry/11330666/1) `FORTRAN77` code that compiles with `GNU` `gfortran` is located in the [csdust3](csdust3/)-directory, next to a copy of the original, unmodified code. The former is compiled with:

```shell
make csdust3
```

This results in a binary executable called `csdust3`. Point the `CSDUST3` environment variable to its location and call it from within `BuI` by running the `run` command. The table below shows the environment variables used by `BuI`.

|variable|function|
---------|--------
|CSDUST3|[CSDUST3](https://mendeley.figshare.com/articles/dataset/CSDUST3_A_radiation_transport_code_for_a_dusty_medium_with_1-d_planar_spherical_or_cylindrical_geometry/11330666/1) executable|
|BUIDIR|`BuI` directory|

A number of example input files are located at [share/bui/examples](share/bui/examples).

## Tools

A number of simple command line tools for dealing with [CSDUST3](https://mendeley.figshare.com/articles/dataset/CSDUST3_A_radiation_transport_code_for_a_dusty_medium_with_1-d_planar_spherical_or_cylindrical_geometry/11330666/1) input and output files are available. These are compiled with:

```shell
make tools
```

This results in a number of `.bin` binary executables in the [tools](tools/)-directory. See the accompanying [README.md](tools/README.md) for details.

Lastly, [share/bui/sm](share/bui/sm) contains a number of [SuperMongo](https://www.astro.princeton.edu/~rhl/sm/) macros that could be useful.

## Notes

1. `BuI` is near feature complete.
2. `BuI` runs on both `Linux` and `MacOS`.
3. Due to a bug in `readline`/`history`, on `Ubuntu` `BuI` segfaults when using command line history search.

## BSD-3 License

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
