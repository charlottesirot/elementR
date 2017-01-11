# elementR <i class="fa fa-star-half-o" aria-hidden="true" align="right"></i>

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/elementR)](https://cran.r-project.org/package=elementR)

## Overview

elementR is an R package facilitating the reduction of elemental microchemistry data from solid-phase LA-ICPMS analysis (laser ablation inductive coupled plasma mass spectrometry). The elementR package provides a reactive and user friendly interface for conducting all steps needed for an optimal data reduction while leaving maximum control for user.

## Technical prerequisites

1. Internet access.

2. For all platforms (Linux, Windows, MacOS), check the version of R installed on your computer (must be at least ≥ 3.2.3, see [https://cran.r-project.org/](https://cran.r-project.org/) for updating your version).

3. For a Macintosh platform, elementR package needs XQuartz. Please check, that XQuartz is installed and runs properly on your computer. If not, please visit https://www.xquartz.org/.

## Installation

The easiest way to get elementR is to install it from [CRAN](https://CRAN.R-project.org/package=elementR):

```
install.packages("elementR", dependencies = T)
library(elementR)
```

Alternatively, you can install the development version (not yet pushed on cran) from [GitHub](https://github.com/charlottesirot/elementR) :

```
#Install elementR's dependencies
pkg <- c("gdata", "shiny","devtools", "shinyjs", "gnumeric", "R6", "shinydashboard", "abind", "stringr", "lmtest", "tcltk", "tcltk2", "reader", "readODS", "readxl")

invisible(vapply(seq_along(pkg), function(x){install.packages(pkg[x], dependencies = T)}))

#Check for devtools
if(!require("devtools")){
	install.packages("devtools")
	library("devtools")
}

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

If, despite the care devoted to programming this package and write this documentation, you have difficulties to install or run elementR, if you have questions about the procedures or calculations, or if you want to report bugs, do not hesitate to consult the official elementR documentation above or on [CRAN](https://CRAN.R-project.org/package=elementR) or connect with us on [GitHub](https://github.com/charlottesirot/elementR).
