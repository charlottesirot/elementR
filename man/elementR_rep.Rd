\name{elementR_rep}
\alias{elementR_rep}
\docType{data}
\title{
Object elementR_rep
}
\description{
The \code{R6Class} object \code{elementR_rep} contains the main information needed for the filtration of a batch of replicates (standard or sample replicates). 
}
\usage{elementR_rep}
\format{
An \code{R6Class} generator object
}

\section{Fields}{
\describe{\item{\code{rep_name}}{A character string corresponding to the name of the considered folder}}
\describe{\item{\code{rep_folderPath}}{A character string corresponding to the path of the considered folder}}
\describe{\item{\code{rep_Files}}{A vector containing the name of the files within the considered folder}}
\describe{\item{\code{rep_data}}{A list containing the \code{elementR_data} corresponding to the replicates included in the considered folder}}
\describe{\item{\code{rep_pas}}{A numerical value corresponding to the time between two consecutive analysis within data of the considered folder}}
}

\section{Methods}{
\describe{\item{\code{setRep_pas()}}{Aim: set \code{rep_pas}}}
\describe{\item{\code{initialize(filePath)}}{Aim: Create and set the basic information of the considered folder; Argument: filePath = the path of the considered folder; Output: an R6Class \code{elementR_rep} object}}
}

\details{
When runElementR is running and as soon as a project is loaded, an \code{elementR_rep} is automatically created for each batch of replicates (i.e. each folder of standards or samples) included in the session. Each of these objects contains the basic information regarding the considered batch (name and path of the folder, the whole data of each replicates) and is filled by the intermediate and final data as user proceeds to the filtration procedure.
}

\examples{
## see elementR_repStandard or elementR_repSample as the creation of elementR_rep depends 
## on the type of data created 
}

\seealso{
  \code{\link{elementR_repStandard}}.
  \code{\link{elementR_repSample}}.
}

