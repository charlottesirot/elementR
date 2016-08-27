\name{elementR_repStandard}
\alias{elementR_repStandard}
\docType{data}
\title{
Object elementR_repStandard
}
\description{
The \code{R6Class} object \code{elementR_repStandard} contains the main information needed for the filtration of a batch of standard replicates. 
}
\usage{delementR_repStandard}
\format{
An \code{R6Class} generator object
}
\section{Inheritance}{The \code{delementR_repStandard} object inherits from the \code{elementR_rep}.}

\section{Fields}{
\describe{\item{\code{rep_type}}{A character string indicating the type of the batch considered (here, "standard")}}
\describe{\item{\code{rep_dataFinale}}{A matrix containing \code{data_standFinalMean} and \code{data_standFinalSD} for all standard replicates included in the considered batch}}
\describe{\item{\code{rep_dataFinaleMean}}{A vector containing the average per chemical element of the \code{rep_dataFinale}}}
\describe{\item{\code{rep_dataFinaleSD}}{A vector containing the standard deviation per chemical element of the \code{rep_dataFinale}}}

}

\section{Methods}{
\describe{\item{\code{setrep_FinalMeanSD()}}{Aim: define and set \code{rep_dataFinaleMean} and \code{rep_dataFinaleSD}}}
\describe{\item{\code{setRep_table(nelem)}}{Aim: define and set \code{rep_dataFinale}; Arguments: nelem = a vector containing the names of the chemical elements to include in the \code{rep_dataFinale}}}
\describe{\item{\code{create()}}{Aim: create and set \code{rep_data} by filling it with the \code{elementR_standard} objects corresponding to standard replicates included in this batch}}
}

\details{
As a subclass object, the \code{elementR_repStandard} object already contains all fields and methods from the  \code{\link{elementR_rep}}. Moreover, it also contains items specifically designed for standard filtration.
}

\examples{
## create a new elementR_repStandard object based on the "filePath" 
## from a folder containing sample replicate

filePath <- system.file("Example_Session/standards", package="elementR")

standBatch <- elementR_repStandard$new(rep_folderPath = filePath)

## Display the files contained in this batch

standBatch$rep_Files
}
\seealso{
  \code{\link{elementR_rep}}.
  \code{\link{elementR_repSample}}.
}
