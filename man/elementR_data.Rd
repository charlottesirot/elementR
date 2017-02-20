\name{elementR_data}
\alias{elementR_data}
\title{Object elementR_data}
\description{
The \code{R6Class} object \code{elementR_data} contains the main information needed for the filtration of a single replicate (sample or standard). 
}
\usage{
elementR_data
}
\format{
An \code{R6Class} generator object
}
\section{Fields}{
\describe{\item{\code{name}}{A character string corresponding to the name of the considered replicate}}
\describe{\item{\code{data}}{A matrix corresponding to the raw data of the considered replicate}}
\describe{\item{\code{fPath}}{A character string corresponding the path of the raw data}}
\describe{\item{\code{bins}}{A numerical value corresponding to the time at which end the blank values}}
\describe{\item{\code{plat}}{A vector containing two numerical values corresponding respectively to the time at which begin and end the plateau values}}
\describe{\item{\code{dataBlank}}{A matrix corresponding to the blank data}}
\describe{\item{\code{dataPlateau}}{A matrix corresponding to the plateau data}}
\describe{\item{\code{dataSuppBlank}}{A matrix corresponding to the data obtained by substracting the averaged blank value (here, \code{BlankAverarge}) from the \code{dataPlateau}}}
\describe{\item{\code{dataSupLOD}}{A matrix of data corresponding to the values of \code{dataSuppBlank} up to the limit of detection (here \code{LOD})}}
\describe{\item{\code{dataNorm}}{A matrix of data corresponding to the values of \code{dataSupLOD} normalized by the chemical element chosen as internal standard (here, \code{elemstand})}}
\describe{\item{\code{elemstand}}{A character string corresponding to the name of the chemical element chosen as internal standard}}
\describe{\item{\code{LOD}}{A vector of numerical values corresponding to the limit of detection for each chemical element of the considered replicate}}
\describe{\item{\code{BlankAverarge}}{A vector of numerical values corresponding to the averaged blank value for each chemical element of the considered replicate}}
\describe{\item{\code{remplaceValue}}{A character string corresponding to the value replacing the \code{dataSuppBlank} below the limit of detection}}
}


\section{Methods}{
\describe{\item{\code{initialize(filePath, sep , dec)}}{Aim: Create and set basic information of the considered replicate; Argument: filePath = the path of the considered replicate data, dec = the decimal system of the data, sep =  the separator character of the data; Output: an R6Class \code{elementR_data} object}}
\describe{\item{\code{setBins(bins)}}{Aim: set \code{bins}; Argument: bins = A numerical value corresponding to the time at which end the blank values}}
\describe{\item{\code{setPlat(plat)}}{Aim: set \code{plat}; Argument: plat = A vector containing two numerical values corresponding respectively to the time at which begin and end the plateau values}}
\describe{\item{\code{setDataBlanc(bins)}}{Aim: set \code{dataBlank}; Argument: bins = A numerical value corresponding to the time at which end the blank values }}
\describe{\item{\code{setDataPlateau(plat)}}{Aim: set \code{dataPlateau}; Argument: plat = A vector containing two numerical values corresponding respectively to the time at which begin and end the plateau values}}
\describe{\item{\code{setDataSuppBlank(bins,plat)}}{Aim: set \code{dataSuppBlank}; Arguments: bins = A numerical value corresponding to the time at which end the blank values, plat = A vector containing two numerical values corresponding respectively to the time at which begin and end the plateau values}}
\describe{\item{\code{setDataSupLOD(bins,plat)}}{Aim: set \code{dataSupLOD}; Arguments: bins = A numerical value corresponding to the time at which end the blank values, plat = A vector containing two numerical values corresponding respectively to the time at which begin and end the plateau values}}
\describe{\item{\code{setDataNorm(bins,plat)}}{Aim: set \code{dataNorm}; Arguments: bins = A numerical value corresponding to the time at which end the blank values, plat = A vector containing two numerical values corresponding respectively to the time at which begin and end the plateau values}}
\describe{\item{\code{reset()}}{Aim: replace \code{dataConcCorr} by NA}}
\describe{\item{\code{OutlierDetectTietjen(x, nbOutliers)}}{Aim: return the place of the outlier of a vector according to Tietjen and outlier methods; Arguments: x = a vector, nbOutliers = the number of suspected outliers; Outputs: a vector of the position of the outlier in the vector}}
\describe{\item{\code{outlierDetection(dat, method, nbOutliers)}}{Aim: return the place of the outlier of a vector; Arguments: dat = a vector, method = the method used for the detection ("Tietjen.Moore Test", "SD criterion", "Rosner's test"), nbOutliers = the number of suspected outliers; Outputs: a vector of the position of the outlier in the vector}}
\describe{\item{\code{detectOutlierMatrix(dat, method, nbOutliers)}}{Aim: return the place of the outlier for each column of a matrix; Arguments: dat = a matrix, method = the method used for the detection ("Tietjen.Moore Test", "SD criterion", "Rosner's test"), nbOutliers = the number of suspected outliers; Outputs: a list of vector corresponding to the position of the outlier in each column of the matrix}}
\describe{\item{\code{outlierReplace(dat, outlierList, rempl)}}{Aim: replace the outliers value of a matrix by rempl; Arguments: dat = a matrix, a list showing the place of the outlier for each column, rempl: the value to replace if outliers}}
\describe{\item{\code{is.possibleOutlier(dat)}}{Aim: check that the vector fits with the needs for outlier detection (length of data > 30 and not all the same); Arguments: dat = a vector of data; OUtputs: TRUE: the investigated vector meets the conditions, FALSE: the investigated vector does not meet the conditions}}
}


\details{
When \code{\link{runElementR}} is running and as soon as a project is loaded, an \code{elementR_data} is automatically created for each replicate included in the session (standard and sample). Each of these objects contains the basic information regarding the considered replicate (name, path and raw data) and is filled by the intermediate and final data as user proceeds to the filtration procedure.
}

\seealso{
  \code{\link{elementR_sample}}.
  \code{\link{elementR_standard}}.
}
\examples{
## create a new elementR_data object based on the "filePath" 
## from a file containing data (accepted format of data: .csv, .ods, .xls, .xlsx)

filePath <- system.file("Example_Session/standards/Stand3.xls", package="elementR")

standard <- elementR_data$new(filePath)

## Display the raw data 

standard$data


}
