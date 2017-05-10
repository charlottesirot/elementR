\name{convertMol_to_PPM}
\alias{convertMol_to_PPM}
\title{convertMol_to_PPM}
\description{
Convert the elemental concentrations (relative to an internal standard) of a data frame from Mol/Mol to ppm/ppm 
}
\usage{
  convertMol_to_PPM(dat, AtomicMass, InternStand)
}

\arguments{
\item{dat}{a data frame of elemental concentrations (relative to an internal standard) in Mol/Mol}   
\item{AtomicMass}{a matrix describing the atomic weight of the elements included in the session}   
\item{InternStand}{the name of the internal standard used in the calculation of the concentrations}   
}

\details{
A matrix describing the atomic weight of the element is included in the present package (AtomicMass.csv). 
Note that \code{convertMol_to_PPM()} will convert the whole data frame uploaded except the first column that is usually dedicted to the time (transect mode) or to the names of the replicate (spot mode) according to \code{runElementR()} method.
}

\examples{
## Convert the file Example1_replicate1.csv included in the package from Mol/Mol into ppm/ppm

# indicate the path and read the file to be converted
filePath <- system.file("Example_conversion/Ex1/Example1_Replicate1.csv", package="elementR")

dat <- readData(filePath, sep = ",", dec = ".")

# indicate the path and read the file containing the atomic weight of the elements
AtomWeightPath <- system.file("AtomicMass.csv", package="elementR")

AtomicMass <- readData(AtomWeightPath, sep = ",", dec = ".")

# set the internal standard
InternStand <- "Ca43"

standard <- convertMol_to_PPM(dat, AtomicMass, InternStand)

## Display the converted data 
standard
}

\seealso{
  \code{\link{convertingReplicate}}.
  \code{\link{convertPPM_to_Mol}}.
}
