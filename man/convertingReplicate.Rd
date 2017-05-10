\name{convertingReplicate}
\alias{convertingReplicate}
\title{convertingReplicate}
\description{
Convert and export the elemental concentrations (relative to an internal standard) from files of a repository from ppt/ppt to Mol/Mol and vice versa while keeping the repository structure.
}
\usage{
  convertingReplicate()
}

\details{
By running \code{convertingReplicate()}, the user has access to an interface through its web browser opened automatlically as soon as the function is launched. This interface allows to upload the repository of the files to be converted, to choose the unit of conversion and to export the final converted data. 
Note that \code{convertingReplicate()} will convert the whole data frame uploaded except the first column that is usually dedicted to the time (transect mode) or to the names of the replicate (spot mode) according to \code{runElementR()} method.  
}

\seealso{
  \code{\link{convertMol_to_PPM}}.
  \code{\link{convertPPM_to_Mol}}.
}


