# CRUD - Payroll (2016)

This is a college work introduced in the PPL discipline (Paradigms of Programming Languages) whose objective is to demonstrate the use of the COBOL language in business software. A small CRUD financial management software was developed, capable of calculating a payroll following the labor laws in 2016 (Brazil).

## Setup

### Dependency

#### Linux

To install you need verify if the following dependencies are installed:

- GNU MP (libgmp) 4.1.2 ou superior
- GNU Libtool (libltdl)

In Ubuntu can install by comand:

```shell
 sudo apt-get install libgmp3-dev libgmp10 libgmp-dev libltdl7 libltdl-dev 
```

To install use the comands below:

```
$ tar -xvzf open-cobol-1.1tar.gz
$ cd open-cobol-1.1
$ ./configure
$ make 
$ sudo make install 
```

​In Ubuntu or others derivatives of Debian we can use this comand: 

```shell
$ sudo apt-get install open-cobol
```

To test if the compiler is working, use this comand: 

```shell
$ cobc -V
```

Result: 

![Result](https://i.imgur.com/X3BoKeb.png?1)

Ready, already we have the compiler, now let's go to install the IDE.

```shell
$ sudo apt-get install python3-pip python-qt4 pyqt4-dev-tools pyqt5-dev pyqt5-dev-tools python-pygments python-setuptools python3-setuptools 
```

Install the pip package management if you don't have installed. Use the comand:

```shell
$ apt install python-pip
```

For install the dependencies remaining use the comands:

```shell
$ pip install pyqode.qt
$ pip install future
$ pip install pyqode.core --upgrade
$ pip3 install pyqode.cobol --upgrade
$ pip install qdarkstyle 
```

Finally, all the dependencies are installed, we have go to the installation of OpenCobolIDE. To do this just type: 

```shell
sudo pip3 install OpenCobolIDE --upgrade
```

## Usage

1. Open OpenCobolIDE
2. Go to preferences and check **Run in external terminal**
3. Open file **payroll.cbl**
4. Click **Run**

## Outputs

![](https://i.imgur.com/lm4BaZZ.png?1)

![](https://i.imgur.com/f7MNjO1.png?1)

