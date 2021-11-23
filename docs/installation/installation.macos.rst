Installation Guide for macOS
****************************

Installing the overspread python package requires python 3.x and a C compiler. Below is a guide for installation.

Install system packages
=======================

In macOS it is recommended that you obtain `gcc` and `python3` using a package manager such as `homebrew` or `macports`. If you choose `homebrew`, then you can install `gcc` and `python` with::

    $ brew install gcc python

This will install the python headers (traditionally supplied by a `python-devel` package in linux) needed to compile the C source code in `overspread`. If you encounter trouble, feel free to post an issue to github: https://github.com/amisr/overspread

Install Python dependencies
===========================

It is generally a good idea to work within a python virtual environment. This isolates the python environment that you work within from the system python enviroment. That means you are free to install packages without worrying about breaking your system. If you don't already have an environment set up, `here's a nice guide <https://realpython.com/python-virtual-environments-a-primer/>`_.

After activating the python environment, you can install some of the dependencies using `pip` like so::

    pip install numpy scipy matplotlib tables pymap3d apexpy

Consult https://github.com/amisr/flipchem for the latest instructions on installing it.

Installing overspread
=====================

Now you can install overspread to the same python environment using `pip`::

    git clone git@github.com:amisr/overspread.git
