\name{elementR_repSample}
\alias{elementR_repSample}
\docType{data}
\title{
object elementR_repSample
}
\description{
The \code{R6Class} object \code{elementR_repSample} contains the main information needed for the filtration of a batch of replicates from the same sample. 
}
\usage{elementR_repSample}
\format{
An \code{R6Class} generator object
}
\section{Inheritance}{The \code{elementR_repSample} object inherits from the \code{elementR_rep}.}

\section{Fields}{
\describe{\item{\code{rep_type}}{A character string indicating the type of the considered batch (here, "sample")}}
\describe{\item{\code{rep_type2}}{A character string corresponding to the processing mode of averaging ("raster" or "spot")}}
\describe{\item{\code{rep_dataFiltre}}{A list containing the data to average of each replicate of the considered sample (\code{dataOutlierFree} for spot mode and \code{dataNorm} for raster mode)}}
\describe{\item{\code{rep_dataFinalSpot}}{A matrix containing the average and the standard deviation per chemical element of the \code{dataOutlierFree} of the final replicates (i.e. chosen to be part of the final calculation)}}
\describe{\item{\code{rep_dataIntermRaster}}{A list containing the realigned \code{dataNorm} of the final replicates (i.e. chosen to be part of the final calculation)}}
\describe{\item{\code{rep_dataFinalRaster}}{A matrix corresponding to the average values of the data contained in \code{rep_dataIntermRaster}}}
\describe{\item{\code{rep_autoCorrel}}{a vector which contains (1) laser diameter, (2) laser speed, (3) which point to keep}}
\describe{\item{\code{rep_dataFinalRasterNonCorr}}{ a matrix of the final data without correlated points}}
}

\section{Methods}{

\describe{\item{\code{setrep_type2(x)}}{Aim: set \code{rep_type2}; Arguments: x = a character string indicating spot or raster mode}}
\describe{\item{\code{Realign2(data, pas)}}{Aim: Realign data; Arguments: data = a list of matrix corresponding to the data to realign, pas = the step of time between two consecutive analysis within data of the considered sample; Output: data = a list of matrix containing the realigned data}}
\describe{\item{\code{setRep_dataFiltre(x)}}{Aim: set \code{rep_dataFiltre}; Arguments: x = a logical value corresponding to the choice of user to correct or not the machine drift}}
\describe{\item{\code{setRep_dataFinalSpot(x)}}{Aim: set \code{rep_dataFinalSpot}; Arguments: x = the matrix to set}}
\describe{\item{\code{intermStepSpot()}}{Aim: create and return an intermediate matrix containing the average and the standard deviation per chemical element for all sample replicates; Output: outputTab = a matrix with two lines corresponding to the average and the standard deviation per chemical element for all sample replicates}}
\describe{\item{\code{intermStepRaster(decalage, input, outliers, replace)}}{Aim: create and return an intermediate matrix containing realigned data for all sample replicates; Inputs: decalage = a vector indicating the translation to operate, input = the data to realign, outliers = a list of outliers, replace = the value to replace in case of outlier, Output: outputList = a list of matrix containing realigned data for all sample replicates}}
\describe{\item{\code{setRep_dataIntermRaster(x)}}{Aim: set \code{setRep_dataIntermRaster}; Arguments: x = the list of matrix to set}}
\describe{\item{\code{setRep_dataFinalRaster()}}{Aim: set \code{rep_dataFinalRaster}}}
\describe{\item{\code{create()}}{Aim: create and set the field \code{rep_data} by filling it with the \code{elementR_sample} objects corresponding to sample replicates included in this batch}}
\describe{\item{\code{set_rep_autoCorrel(x)}}{Aim: set \code{rep_autoCorrel}, Input: x = the value to set}}
\describe{\item{\code{set_rep_dataFinalRasterNonCorr()}}{Aim: set \code{rep_dataFinalRasterNonCorr}}}
\describe{\item{\code{RealignCol(dat1, dat2, col, step)}}{Aim: realign two tables according to a chosen column (based on a convolution); Inputs: dat1 & dat2 = matrix to realign, col = the column to realign, step = the step between two consecutive analysis; Outputs: the realign data}}
\describe{\item{\code{RealignColList(listRealig, col, step)}}{Aim: realign a list of matrix according to a chosen column (based on a convolution); Inputs: listRealig = a list of matrix to realign, col = the column to realign, step = the step between two consecutive analysis; Outputs: the realign data}}
\describe{\item{\code{RealignAll(dat1, dat2, step)}}{Aim: realign a list of matrix according to all columns (based on a convolution); Inputs: dat1 & dat2 = matrix to realign, step = the step between two consecutive analysis; Outputs: the realign data}}
\describe{\item{\code{RealignListAll(listRealig, step)}}{Aim: realign a list of matrix according to all columns (based on a convolution); Inputs: listRealig = a list of matrix to realign, step = the step between two consecutive analysis; Outputs: the realign data}}
}

\details{
As a subclass object, the \code{elementR_sample} object already contains all fields and methods from the  \code{\link{elementR_rep}}. Moreover, it also contains items specifically designed for sample filtration.
}
\examples{
## create a new elementR_sample object based on the "filePath" 
## from a folder containing sample replicate

filePath <- system.file("Example_Session/samples/Sample_1", package="elementR")

sampleBatch <- elementR_repSample$new(filePath)

## Display the data contained in this batch

sampleBatch$rep_data
}
\seealso{
  \code{\link{elementR_rep}}.
  \code{\link{elementR_repStandard}}.
}
