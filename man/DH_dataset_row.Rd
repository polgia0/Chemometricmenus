\name{DH_dataset_row}
\alias{DH_dataset_row}
\title{Dataset modification row wise}
\description{Allow row deleting, copying, renaming etc.}
\usage{DH_dataset_row(previous.name = "")}
\arguments{
  \item{previous.name}{Default name for the new dataframe, optional}
}
\details{
\figure{DHdatasetmenu.png}
The following sub-options are present:
\itemize{
  \item Row
  \item Column
}
They allow to perform specific tasks (Delete, Extract, Column/Row Name, Copy) on specific rows or columns in a matrix.
This option does not work on dataframes. This option performs one single operation (e.g., if n rows must be deleted, 
it has to be called n times), and is therefore useful only for minor changes; for more extensive changes it is suggested 
to modify the dataset by using an external editor such as a spreadsheet program, and load the file again in R after all 
the changes have been made.\cr
Caution: the original selected object will be overwritten by the modified object.\cr
After having selected the data matrix, the following dialogue box will be shown, in which the row/column number and the 
operation to be performed must be specified.\cr
\itemize{
\item \bold{Data Set-Row/Column-Delete}\cr
The selected row/column is deleted. If more than one row/column must be deleted, it is suggested to do it in descending
 order (if rows 12 and 15 must be deleted, after have deleted row 12 the former row 15 will be row 14).
\item \bold{Data Set-Row/Column-Extract}\cr
The selected row/column is removed from the matrix and saved in the variable ex.row (or ex.col).
\item \bold{Data Set-Row/Column-Column/Row Names}\cr
The selected row/column is removed from the data table and its elements are considered as the column/row names. 
For a row being transformed into column names see the limitations about the data files reported at the beginning of
 this help. For a column being transformed into row names, this option is allowed only if the column complies with 
 the R rules of name definition (e.g., no duplicate names are allowed).
\item \bold{Data Set-Row/Column-Copy}\cr
The selected row/column is saved in the variable ex.row (or ex.col). It is the same as the \emph{Extract} sub-option, 
with the difference that in this case the selected row/column is not removed from the matrix.}
}
\value{New objects are created in the memory depending of the user choice.}
\references{}
\author{Riccardo Leardi and Gianmarco Polotti with contributions from Giorgio Marubini.Gruppo di Chemiometria (Divisione di Chimica Analitica della Societa' Chimica Italiana)}
\note{}
\seealso{}
\examples{}
\keyword{ dataset }
