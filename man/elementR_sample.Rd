\name{elementR_sample}
\alias{elementR_sample}
\docType{data}
\title{
Object elementR_sample
}
\description{
The \code{R6Class} object \code{elementR_sample} contains the main information needed for the filtration of a single sample replicate. 
}
\usage{elementR_sample}
\format{
An \code{R6Class} generator object
}
\section{Inheritance}{The \code{elementR_sample} object inherits from the \code{elementR_data}}

\section{Fields}{
\describe{\item{\code{type}}{A character string corresponding to the type of replicate (here, "sample")}}
\describe{\item{\code{dataConc}}{A matrix corresponding to the \code{dataNorm} converted in concentration}}
\describe{\item{\code{dataConcCorr}}{A matrix corresponding to the \code{dataConc} corrected (or not) from the machine drift}}
}

\section{Methods}{
\describe{\item{\code{setDataConc(bins, plat, calibFile, meanStand, rempl)}}{Aim: set \code{dataConc}; Arguments: bins = a numerical value corresponding to the time at which end the blank values, plat = a vector of two numerical values corresponding respectively to the time at which begin and end the plateau, calibFile = a matrix corresponding to the data of the calibration file, meanStand = a vector containing the averaged signal intensity per chemical element for all standard replicates of the running session, rempl = the value replacing data if below the limit of detection}}

\describe{\item{\code{setDataConcCorr(bins, plat, name, calibFile, meanStand, rankSample, rankStandard, model, correction, rempl, threshold)}}{Aim: set \code{dataConcCorr}; Arguments: bins = a numerical value corresponding to the time at which end the blank values, plat = a vector of two numerical values corresponding respectively to the time at which begin and end the plateau, name = a character string corresponding to the name of the sample replicates, calibFile = a matrix corresponding to the the calibration file, meanStand = a vector containing the averaged signal intensity per chemical element for all standard replicates of the running session, rankSample = a vector containing the rank of each sample in ICPMS analysis, rankStandard = a vector containing the rank of each standard in ICPMS analysis, correction = a vector indicating the chemical elements to correct from machine drift, model = a matrix containing the parameters of the linear regression corresponding to the machine drift for all chemical elements, threshold = the R2 threshold to consider that the model does not fit to a linear model}}

\describe{\item{\code{renderData(curve)}}{Aim: render data without proceding to their calculation; Argument: curve = a character string corresponding to the type of data to render ("Blank" for calculate and/or render the \code{dataBlank}, "Raw" for \code{data}, "Plateau" for \code{dataPlateau}, "Blank removed" for \code{dataSuppBlank}, ">LOD" for \code{dataSupLOD}, "Normalized" for \code{dataNorm}, "Concentration" for \code{dataConc} and "Conc. corrected" for \code{dataConcCorr}); Output: a matrix of the required data}}

\describe{\item{\code{getData(curve, bins, plat, name, calibFile, meanStand, rankSample, rankStandard, model, correction)}}{Aim: calculate and render the required data ; Arguments: curve = a character string corresponding to the type of data to calculate (for more details, see renderData arguments), bins = a numerical value corresponding to the time at which end the blank values, plat = a vector of two numerical values corresponding respectively to the time at which begin and end the plateau, name = a character string corresponding to the name of the sample replicates, calibFile = a matrix corresponding to the the calibration file, meanStand = a vector containing the averaged signal intensity per chemical element for all standard replicates of the running session, rankSample = a vector containing the rank of each sample in ICPMS analysis, rankStandard = a vector containing the rank of each standard in ICPMS analysis, correction = a vector indicating the chemical elements to correct from machine drift, model = a matrix containing the parameters of the linear regression corresponding to the machine drift for all chemical elements, threshold = the R2 threshold to consider that the model does not fit to a linear model}}
}

\details{
As a subclass object, the \code{elementR_sample} object already contains the whole fields and methods from the \code{\link{elementR_data}}. Moreover, it also contains items specifically designed for sample filtration.
}

\seealso{
  \code{\link{elementR_data}}.
  \code{\link{elementR_standard}}.
}

\examples{
## create a new elementR_sample object based on the "filePath" from a file containing data
## replicate (accepted format of data: .csv, .ods, .xls, .xlsx)

filePath <- system.file("Example_Session/samples/Sample_1/Sample1_Rep1.csv", package="elementR")

sampleExample <- elementR_sample$new(filePath)

## Display the name of the object 

sampleExample$name


}
