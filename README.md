# elementR :star:

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/elementR)](https://cran.r-project.org/package=elementR)

## Overview

**elementR** is an R package facilitating the reduction of elemental microchemistry data from solid-phase [LA-ICPMS](https://en.wikipedia.org/wiki/Inductively_coupled_plasma_mass_spectrometry) analysis (laser ablation inductive coupled plasma mass spectrometry). The elementR R-package provides a reactive and user friendly interface running in the web browser for conducting all steps needed for an optimal data reduction while leaving maximum control to the user.

**elementR** implements [R6 R classes](https://github.com/wch/R6/) and is based on [Shiny](http://shiny.rstudio.com/) for it's GUI.

#### Technical prerequisites

1. Internet access. :laughing:

2. For all platforms (Linux, Windows, MacOS), check the version of R installed on your computer (`sessionInfo()`), must be ≥ 3.2.3, see [https://cran.r-project.org/](https://cran.r-project.org/) for updating your version.

3. For Mac users, elementR package needs XQuartz. Please check, that XQuartz is installed and runs properly on your computer. If not, please visit [https://www.xquartz.org/](https://www.xquartz.org/).

## Installation

The easiest way to get elementR is to install it from [CRAN](https://CRAN.R-project.org/package=elementR):

```
install.packages("elementR", dependencies = T)
library(elementR)
```

Alternatively, you can install the development version (not yet pushed on cran) from [GitHub](https://github.com/charlottesirot/elementR) :

```
#Install elementR's dependencies
pkgs <- c("gdata", "shiny","devtools", "shinyjs", "gnumeric", "R6", "shinydashboard", "abind", "stringr", "lmtest", "tcltk", "tcltk2", "reader", "readODS", "readxl")

invisible(lapply(pkgs, function(pkg){
	if(!require(pkg)){
		install.packages(pkg, dependencies=TRUE)
	}
}))

#Install elementR
devtools::install_github("charlottesirot/elementR", ref = "master", force = T, dependencies = T)

library(elementR)
```

### Notes on installation :

> For Mac users: during installation elementR will ask to install "XCode/otool". Accept this installation.


> If the installation is stuck at the installation of the tcltk package, this probably means that XQuartz does not run properly. Check its validity.


> elementR runs on any web browser. However, as the graphic of the user interface has been developed based on Firefox, authors highly recommend to run elementR under [Firefox](https://www.mozilla.org/en-US/firefox/new/).


## Usage

For launching elementR, just run the following command in your R console:

```
runElementR()
```

## Learning elementR

If you are new to elementR you are better off starting with a systematic introduction, this can be found in the [pdf documentation](inst/elementR_documentation.pdf) of this package.

## Troubleshoutting

If, despite the :heart: brought during the programming of this R package and writing of this documentation, you have difficulties to install or run elementR, if you have questions about the procedures or calculations, or if you want to report bugs :beetle:, do not hesitate to consult the official elementR documentation above or on [CRAN](https://CRAN.R-project.org/package=elementR) or connect with us on [GitHub](https://github.com/charlottesirot/elementR).
