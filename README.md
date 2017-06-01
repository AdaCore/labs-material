AdaCore University Labs - Support Library
=========================================

This repository hosts source files for the support library to be used in
[AdaCore University's courses](http://university.adacore.com/).


Build & Installation
--------------------

In order to build and install it, run from the top directory:

```sh
$ make PREFIX=/path/to/install
```

For ease of use, we recommend to install it in the same directory as GNAT and
GPS. Otherwise, make the library available for GPRbuild:

```sh
export GPR_PROJECT_PATH=/path/to/install/share/gpr:$GPR_PROJECT_PATH
```

License
-------

This material is licensed under the terms of the GPLv3 (General Public License
version 3). See [COPYING3](COPYING3) for more information.
