\name{readData}
\alias{readData}
\title{readData}
\description{
Read the content of an Excel (.xls and .xlsx), OpenOffice (.ods) and text (.csv) worksheet  
}
\usage{
  readData(x)

}

\arguments{
\item{x}{a character string corresponding to the path or name of the file to read}   
}

\details{
For the Excel and text format, \code{readData} reads by default the first worksheet of the file and the one called "data" for the OpenOffice format.
}


\examples{
## Read data based on its path "filePath"

filePath <- system.file("Example_Session/standards/Stand3.csv", package="elementR")

readData(filePath)

}

