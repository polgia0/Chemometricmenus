\name{DH_export_CSV}
\alias{DH_export_CSV}
\title{Export a Dataset in CSV}
\description{Export a Dataset in CSV format}
\usage{DH_export_CSV(previous.name = "")}
\arguments{
  \item{previous.name}{Default name for the new dataframe, optional}
}
\details{
\figure{DHexportmenu.png}\cr
The \bold{Export} option allows the user to export an object of the workspace, saving it as a file. 
The following sub-options are present:
\itemize{
\item CSV
\item TXT
}
A dialogue box asks for the name of the object,  the characters to be used as field separator and decimal 
separator, ad the coding for the missing data.
The default settings are \emph{;} as field separator, \emph{.} as decimal separator, \emph{NA} for missing data.
The file, with the same name of the object and the selected extension, will be created in the \emph{working} directory.
If neither the \emph{Header} nor the \emph{Row Names} boxes are checked, then only the data matrix will be saved.
If the \emph{Header} box is checked, then the first row of the file will be the header of the columns.
If the \emph{Row Names} box is checked, then the first column of the file will be the name of the rows.
If both the \emph{Header} box and the \emph{Row Names} box are checked, then the first row of the file 
will be the header of the columns and the first column of the file will be the name of the rows
 (obviously, the fist cell of the first row will be empty).
}
\value{A new file is created with the internal matrix}
\references{}
\author{Riccardo Leardi and Gianmarco Polotti with contributions from Giorgio Marubini.Gruppo di Chemiometria (Divisione di Chimica Analitica della Societa' Chimica Italiana)}
\note{}
\seealso{}
\examples{}
\keyword{ export }
