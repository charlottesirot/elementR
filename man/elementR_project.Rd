\name{elementR_project}
\alias{elementR_project}
\docType{data}
\title{
Object elementR_project
}
\description{
The \code{R6Class} object \code{elementR_project} contains all the information needed for running an elementR session
}
\usage{elementR_project}
\format{
An \code{R6Class} generator object
}

\section{Fields}{

\describe{\item{\code{name}}{A character string corresponding to the name of the project}}
\describe{\item{\code{folderPath}}{A character string corresponding to the path of the project}}
\describe{\item{\code{standardsPath}}{A character string corresponding to the path of the standard folder}}
\describe{\item{\code{standardsFiles}}{A vector containing the names of each standard file}}
\describe{\item{\code{standards}}{A list containing the \code{\link{elementR_repStandard}} of each type of standard}}
\describe{\item{\code{samplesPath}}{A character string corresponding to the path of the sample folder}}
\describe{\item{\code{samplesFiles}}{A vector containing the names of each sample file}}
\describe{\item{\code{samples}}{A list containing the \code{\link{elementR_repSample}} of each sample}}
\describe{\item{\code{EtalonPath}}{A character string corresponding to the path of the calibration file}}
\describe{\item{\code{EtalonData}}{A matrix corresponding to the calibration data}}
\describe{\item{\code{listeElem}}{A vector containing the names of the chemical elements included in the project}}
\describe{\item{\code{flag_stand}}{A vector indicating which standards have been filtered}}
\describe{\item{\code{flag_Sample}}{A vector indicating which samples have been filtered}}
\describe{\item{\code{flagRealign}}{A list vectors indicating which samples have been realigned or averaged}}
\describe{\item{\code{standardRank}}{A vector corresponding to the standard rank in ICPMS analysis}}
\describe{\item{\code{sampleRank}}{A vector corresponding to the sample rank in ICPMS analysis}}
\describe{\item{\code{elementChecking}}{A list indicating the number and the location of the error(s) of structure within data included in the project}}
\describe{\item{\code{errorSession}}{A numerical value indicating the non numeric error(s) within data included in the project}}
\describe{\item{\code{regressionModel}}{A matrix summarizing, for each chemical element, the parameters of the linear regression corresponding to the machine drift}}
\describe{\item{\code{machineCorrection}}{A vector summarizing the chemical element(s) to correct from machine drift}}
\describe{\item{\code{flagMachineCorrection}}{A numerical value indicating the validation of the machine correction step}}
\describe{\item{\code{nbCalib}}{A vector corresponding to the number of standard values available for each chemical element to proceed the linear regression}}
\describe{\item{\code{elemStand}}{A character string indicating the chemical element considered as internal standard (by default = Ca)}}
\describe{\item{\code{summarySettings}}{A matrix summarizing all the parameters set by user for each replicate (sample and standard)}}
\describe{\item{\code{ChoiceUserCorr}}{A logical value corresponding to the choice of the user to correct or no the session based on the first step of configuration}}
\describe{\item{\code{R2Threshold}}{the threshold to switch the machine drift correction from a linear to a neighbor correction}}
}

\section{Methods}{

\describe{\item{\code{set_summarySettings(name, rank, bins, plat1, plat2, average, LOD)}}{ Aim: set \code{summarySettings}; Arguments: name = a character string corresponding to the name of the replicate to set, rank= its rank in ICPMS analysis, bins = a numerical value corresponding to the time at which end the blank values, plat1 = a numerical value corresponding to the time at which begin the plateau values, plat2 = a numerical value corresponding to the time at which end the plateau values, average =  a vector corresponding to the blank averaged value (here, \code{BlankAverarge}) for each chemical element of the considered replicate, LOD = a vector corresponding to the limit of detection (here, \code{LOD}) for each chemical element of the considered replicate}}
\describe{\item{\code{is.integer0(x)}}{Aim: test the integer(0); Arguments: x = a vector to test; Outputs:  TRUE or FALSE}}
\describe{\item{\code{closest(x,y)}}{Aim: find the nearest value among a vector of numerical data; Arguments: x = a vector of numerical values, y = the investigated value; Output: val = a list of two values: the nearest value and its place within the vector}}
\describe{\item{\code{PlotIC(name, Mean,SD, coord, lengthSeg, xlim, ylim, type = "p", xlab, ylab)}}{Aim: plot mean +/- SD; Arguments: name = a vector of the names to display on xaxis, Mean = a vector of mean, SD = a vector of SD, coord = a vector of coordonnates to place xticks, lengthSeg = a numeric value cooresponding to the length of the top segment of the SD bar, xlim & ylim = the limits of plots, xlab & ylab = the labels of axis}}
\describe{\item{\code{setEtalon(x, sep, dec)}}{Aim: define \code{EtalonPath} and \code{EtalonData} and check the validity of their data structure; Arguments: x = a character string corresponding to the path of the calibration file, dec = the decimal system of the data, sep =  the separator character of the data}}
\describe{\item{\code{setflagMachineCorrection(x)}}{Aim: set \code{flagMachineCorrection}; Arguments: x = the numerical value to set}}
\describe{\item{\code{NonNumericCheck(data, col)}}{Aim: check non numeric characters of data; Arguments: data = a dataframe or a matrix, col = a vector of numerical values corresponding to the column(s) to investigate; Output: errB = a numerical value corresponding to the number of cells containing non numeric characters}}
\describe{\item{\code{setflagStand(place, value)}}{Aim: set \code{flag_stand}; Arguments: place = a numerical value corresponding to the considered replicate, value = the numerical value to set}}
\describe{\item{\code{setflagSample(sample, replicate, value)}}{Aim: set \code{flag_Sample}; Arguments: sample = a numerical value corresponding to the considered sample, replicate = a numerical value corresponding to the considered replicate, value = the numerical value to set}}
\describe{\item{\code{setCorrection(x)}}{Aim: set \code{machineCorrection}; Arguments: x = a vector indicating the chemical elements to correct from machine drift}}
\describe{\item{\code{correction()}}{Aim: proceed to the linear regression on standards replicates and set \code{nbCalib} & \code{regressionModel}}}
\describe{\item{\code{setRank(type, value)}}{Aim: set the order in which ICPMS runs each standard (\code{standardRank}) and sample (\code{sampleRank}) replicates; Arguments: type = a character string indicating the type of replicate standard ("standard") or sample ("sample"), value = a numerical value corresponding to the rank of the considered replicate}}
\describe{\item{\code{set_flagRealign(replicate, type, value)}}{Aim: set \code{flagRealign}; Arguments: replicate = a numerical value corresponding to the number of the considered replicate, type = a character string indicating the raster or spot mode, value = the numerical value to set}}
\describe{\item{\code{setElemStand(elem)}}{Aim: define \code{elemStand} and transmit this value to all \code{elementR_rep} and \code{elementR_data} objects inlcuded in the project; Arguments: elem = a character string corresponding to the element considered as intern standard}}
\describe{\item{\code{initialize(folderPath, sep, dec)}}{ Aim: create the project; Arguments: filePath = the path of the considered project, dec = the decimal system of the data, sep =  the separator character of the data; Outputs: R6Class \code{elementR_project}}}
\describe{\item{\code{set_ChoiceUserCorr(x)}}{ Aim: information about the will of user to check or not the machine drift; Arguments: x = T (for checking machine drift), F (for not checking machine drift) }}
\describe{\item{\code{setR2Threshold(x)}}{ Aim: set \code{R2Threshold}; Arguments: x = a value between 0 and 1}}
\describe{\item{\code{insert.at(a, pos, toInsert)}}{ Aim: insert values in vectors; Arguments: a = a vector, pos =  the position to insert,  toInsert = a vector to insert}}
\describe{\item{\code{detectPlateau(dat, col)}}{ Aim: detection of the plateau limits of a matrix based on clustering methods and on the internal standard element; Arguments: dat = the data to proceed, col =  the column used for the detection }}
\describe{\item{\code{detectBlank(dat, col)}}{ Aim: detection of the blank limits of a matrix based on the derivative value and on the internal standard element; Arguments: dat = the data to proceed, col =  the column used for the detection }}
}

\details{
The \code{elementR_project} structure allows to organized data in a session framework, faciliting therefore numerous major functionalities: handling as many standard replicates as wanted, machine drift verification and correction, sample replicate realignment and averaging. Moreover, this object can be easily exported, allowing user to re-open it later for finishing or editing final results.
}
\examples{
## create a new elementR_repStandard object based on the "filePath" 
## from a folder containing sample replicate

filePath <- system.file("Example_Session", package="elementR")

exampleProject <- elementR_project$new(filePath)

## Display the raw data 

exampleProject$samplesFiles

}
\seealso{
  \code{\link{elementR_rep}}.
  \code{\link{elementR_data}}.
}

