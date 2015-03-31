# Introduction #

This tracks the release process for version 1.4
It should include tested compilers/platforms.

# Tested Platforms #

| **Name** | **Date**| **64bit** | **32bit** | **compiler** | **options** | **Notes** |
|:---------|:--------|:----------|:----------|:-------------|:------------|:----------|
|OS X 10.7|2013-03-13| x | n/a |gcc-4.2.1 (LLVM) |  _none_ |  build ok/tests fail 1) |
| Debian 5.0 (lenny) | 2013-03-13 | x |  | gcc-4.3.2 | WCSLIB\_INCLUDE\_DIR=/usr/local/include/cfistio | builds ok/tests fail(1) |

# Comments #

(1) **Failed tests**: tAntennaResponses, tEarth`*`
> tAntennaResponses possibly due to different measures observatories table