\name{elementR_standard}
\alias{elementR_standard}
\docType{data}
\title{
Object elementR_standard
}
\description{
The \code{R6Class} object \code{elementR_standard} contains the main information needed for the filtration of a single standard replicate. 
}
\usage{elementR_standard}
\format{
An \code{R6Class} generator object
}
\section{Inheritance}{The \code{elementR_standard} object inherits from the \code{elementR_data}.}

\section{Fields}{

\describe{\item{\code{type}}{A character string indicating the type of replicate (here, "standard")}}
\describe{\item{\code{dataOutlierFree}}{A matrix corresponding to the \code{dataNorm} without abnomalities}}
\describe{\item{\code{data_standFinalMean}}{A vector corresponding to the average of \code{dataOutlierFree} per chemical element}}
\describe{\item{\code{data_standFinalSD}}{A vector corresponding to the standard deviation of \code{dataOutlierFree} per chemical element}}
}

\section{Methods}{

\describe{\item{\code{setDataOutlierFree(bins, plat)}}{Aim: set \code{dataOutlierFree}; Arguments: bins = a numerical value corresponding to the time at which end the blank values, plat = a vector of two numerical values corresponding respectively to the time at which begin and end the plateau}}
\describe{\item{\code{setdata_standFinal()}}{Aim: set \code{data_standFinalMean} and \code{data_standFinalSD}}}
\describe{\item{\code{renderData(curve)}}{Aim: render data without proceeding to their calculation; Argument: curve = a character string corresponding to the type of data to render ("Blank" for \code{dataBlank}, "Raw" for \code{data}, "Plateau" for \code{dataPlateau}, "Blank removed" for \code{dataSuppBlank}, ">LOD" for \code{dataSupLOD}, "Normalized" for \code{dataNorm}, "Outliers free" for \code{dataOutlierFree}); Output: a matrix of the required data}}
\describe{\item{\code{getData(curve, bins, plat)}}{Aim: calculate and render the required data ; Arguments: curve = a character string corresponding to the type of data to render (for more details, see the curve argument of the renderData function), bins = a numerical value corresponding to the time at which end the blank values, plat = a vector of two numerical values corresponding respectively to the time at which begin and end the plateau; Output: a matrix of the required data}}
}


\details{
As a subclass object, the \code{elementR_standard} object already contains all fields and methods from the \code{\link{elementR_data}}. Moreover, it also contains items specifically designed for standard filtration.
}

\seealso{
  \code{\link{elementR_data}}.
  \code{\link{elementR_sample}}.
}

\examples{
## create a new elementR_standard object based on the "filePath" from a file containing data

filePath <- system.file("Example_Session/standards/Stand1.csv", package="elementR")

standardExample <- elementR_standard$new(filePath)

## Display the raw data 

standardExample$data

}
