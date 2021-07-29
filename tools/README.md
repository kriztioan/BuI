# CSDUST3 Tools

## Description

`CSDUST3 Tools` is a collection of simple command line tools for dealing with [CSDUST3](https://mendeley.figshare.com/articles/dataset/CSDUST3_A_radiation_transport_code_for_a_dusty_medium_with_1-d_planar_spherical_or_cylindrical_geometry/11330666/1) input and output files. Many of them rely on input/output redirection.

For some of the tools example input/output files can be found in its `tools`' `examples`-subdirectory.

## Usage

All tools are simultaneously compiled, from the `tools`-directory, by calling

```shell
make
```

A specifc tool alone can be compiled by calling `make` from the `tools`'-directory.

The above either results in a number of `.bin` binary executables or a single binary executable in the [tools](tools/)-directory.

Below a table of tools and a brief description of their usage.

|tool|description|
|----|-----------|
|12to9.bin|clip characters 5-7|
|aline.bin|reformat to 8-column data|
|conv.bin|do convolution|
|dust.bin|caclulate dust properties|
|e2d.bin|covert 'e's to 'd's|
|grid.bin|make wavelength grid|
|interp.bin|interpolate data (linear)|
|interplog.bin|interpolate data (log)|

## Notes

1. A number of these tools are likely to be implemented directly into [BuI](../README.md); a command line user interface to [CSDUST3](https://mendeley.figshare.com/articles/dataset/CSDUST3_A_radiation_transport_code_for_a_dusty_medium_with_1-d_planar_spherical_or_cylindrical_geometry/11330666/1).

## BSD-3 License

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
