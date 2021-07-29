# CSDUST3

## Description

[CSDUST3](https://mendeley.figshare.com/articles/dataset/CSDUST3_A_radiation_transport_code_for_a_dusty_medium_with_1-d_planar_spherical_or_cylindrical_geometry/11330666/1) is the third version of `DUSTCD` (*Spagna+* [1982](http://dx.dio.org/10.1016/0010-4655(83)90032-2)) and implements the radiative transfer model from *Leung*
([1975](http://dx.doi.org/10.1086/153697*), [1976b](http://dx.doi.org/10.1086/154694)). It is written by *Egan+* ([1998](https://doi.org/10.1016/0010-4655(88)90047-1)) in `FORTRAN77`.

A research report that utilizes [CSDUST3](https://mendeley.figshare.com/articles/dataset/CSDUST3_A_radiation_transport_code_for_a_dusty_medium_with_1-d_planar_spherical_or_cylindrical_geometry/11330666/1) and that makes use of the command line user interface `BuI` to interact with it can be found in the `Bui` [docs](../docs/).

## Usage

 A modified copy of the [CSDUST3](https://mendeley.figshare.com/articles/dataset/CSDUST3_A_radiation_transport_code_for_a_dusty_medium_with_1-d_planar_spherical_or_cylindrical_geometry/11330666/1) `FORTRAN77` code that compiles with `GNU` `gfortran` is located in the [csdust3](csdust3/)-directory, next to a copy of the original, unmodified code. The former is compiled with:

```shell
make
```

This results in a binary executable called `csdust3`, located in parent directory. It relies on input/output redirection and is run as:

```shell
./csdust3 < input_file > output_file
```

## Notes

1. `GNU` FORTRAN compiler required.
