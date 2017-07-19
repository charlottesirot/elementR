# elementR :star:

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/elementR)](https://cran.r-project.org/package=elementR)
[![Download_badge](http://cranlogs.r-pkg.org/badges/grand-total/elementR?color=brightgreen)](https://cran.r-project.org/package=elementR)
[![Build Status](https://api.travis-ci.org/charlottesirot/elementR.svg?branch=master)](https://travis-ci.org/charlottesirot/elementR.svg?branch=master)
[![codecov](https://codecov.io/gh/charlottesirot/elementR/branch/master/graph/badge.svg)](https://codecov.io/gh/charlottesirot/elementR)

## Overview

**elementR** is an R package facilitating the handling and reduction of elemental microchemistry data obtained from solid-phase [LA-ICPMS](https://en.wikipedia.org/wiki/Inductively_coupled_plasma_mass_spectrometry) analysis (laser ablation inductive coupled plasma mass spectrometry). The **elementR** R-package provides:

- a set of classes to handle LA-ICPMS data
- a reactive user friendly interface running in the web browser to conduct all steps needed for an optimal data reduction while leaving maximum control to the user.

**elementR** implements [R6 R classes](https://github.com/wch/R6/) and is based on [Shiny](http://shiny.rstudio.com/) for it's GUI.

#### Before installation

1. Check the version of R installed on your computer (`sessionInfo()`), must be ≥ 3.2.3, see [https://cran.r-project.org/](https://cran.r-project.org/) to update your version.

2. __For Mac users only__: elementR package needs XQuartz. Please check, that XQuartz is installed and runs properly on your computer. If not, please visit [https://www.xquartz.org/](https://www.xquartz.org/).

## Installation

The easiest way to get elementR is to install it from [CRAN](https://CRAN.R-project.org/package=elementR):

```
install.packages("elementR", dependencies = T)
library(elementR)
```

Alternatively, you can install the development version (not yet pushed on cran) from [GitHub](https://github.com/charlottesirot/elementR) :

```
#Install elementR's dependencies
pkgs <- c("gdata", "shiny","devtools", "shinyjs", "gnumeric", "R6", "shinydashboard",
	"abind", "stringr", "lmtest", "tcltk", "tcltk2", "reader", "readODS", "readxl", "outliers", "climtrends",
	"EnvStats", "colourpicker", "zoo")

invisible(lapply(pkgs, function(pkgs){
	if(!require(pkgs)){
		install.packages(pkgs, dependencies=TRUE)
	}
}))

#Install elementR
devtools::install_github("charlottesirot/elementR", ref = "master", force = T, dependencies = T)

library(elementR)
```

### Notes on installation :

> For Mac users: during installation elementR will ask to install "XCode/otool". Accept this installation.

> If the installation is stuck at the installation of the tcltk package, this probably means that XQuartz does not run properly. Check its validity.

> For Linux users: you could need to install additional libraries: 
```
sudo apt-get update
sudo apt-get install libcurl4-openssl-dev
sudo apt-get install libssl-dev
sudo apt-get install libxml2-dev
sudo apt-get install libssh2-1-dev
```

> the elementR GUI runs on any web browser. However, as the graphic of the user interface has been developed based on Firefox, authors highly recommend to run elementR under [Firefox](https://www.mozilla.org/en-US/firefox/new/).


## Usage

To launch elementR, run the following command in your R console:

```
runElementR()
```

## Learning elementR

If you are new to elementR you are better off starting with a systematic introduction, this can be found in the [pdf documentation](inst/elementR_documentation.pdf) of this package.

## Troubleshoutting

If, despite the :heart: brought during the programming of this R package and writing of this documentation, you have difficulties to install or run elementR, if you have questions about the procedures or calculations, or if you want to report bugs :beetle:, do not hesitate to consult the official elementR documentation above or on [CRAN](https://CRAN.R-project.org/package=elementR) or connect with us on [GitHub](https://github.com/charlottesirot/elementR).
