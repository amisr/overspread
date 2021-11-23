Installation Guide for Linux
****************************

Installing the overspread python package requires python 3.x and a C compiler. Below is a guide for installation.

Install system packages
=======================

Use you linux system package manager, for example `dnf` on Fedora or `aptitude` in Ubuntu, to install::

    gcc python3 python3-devel

You can usually find the OS specific package name with some light googling. If not, feel free to post an issue to github: https://github.com/amisr/overspread

Install numpy
=============

It is generally a good idea to work within a python virtual environment. This isolates the python environment that you work within from the system python enviroment. That means you are free to install packages without worrying about breaking your system. If you don't already have an environment set up, `here's a nice guide <https://realpython.com/python-virtual-environments-a-primer/>`_.

After activating the python environment, you can install numpy using `pip`::

    pip install numpy scipy matplotlib tables pymap3d apexpy

Consult https://github.com/amisr/flipchem for the latest instructions on installing it.

Installing overspread
=====================

Now you can install overspread to the same python environment using `pip`::

    git clone git@github.com:amisr/overspread.git
