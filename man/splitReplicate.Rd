\name{splitReplicate}
\alias{splitReplicate}
\title{splitReplicate}
\description{
Split a single file corresponding to a whole session (Excel (.xls and .xlsx), OpenOffice (.ods) and text (.csv) worksheet) in separate files containing each only one replicate in order to fit to runElementR requirment 
}
\usage{
  splitReplicate()
}

\details{
By running \code{splitReplicate()}, the user has access to an interface through its web browser opened automatlically as soon as the function is launched. This interface allows to upload the file to split (sep = ";", dec = "."). The function helps the user to divide the file thanks to a clustering method (kmean). At the end, the user has the possibility to export the split data in the chosen directory (export in .csv).
}


