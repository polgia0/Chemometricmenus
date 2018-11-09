\name{DH_workspace_management}
\alias{DH_workspace_management}
\title{Menu to handle matrix objects}
\description{Easily allow edit,copy,delete,... matrices in the workspace}
\usage{DH_workspace_management(previous.name = "")}
\arguments{
  \item{previous.name}{Default name for the new dataframe, optional}
}
\details{
This option allows to  perform several tasks on the objects stored in the workspace.
When selecting it, a dialogue box shows the list of the objects, their type and size.
After having selected an object, the user can perform different operations by clicking on 
one of the buttons at the bottom.\cr
Note: the dialogue box can be closed only by hitting the \emph{Close} button.\cr
\cr 
\figure{DHworkspacemanagement.png}\cr
\cr
\itemize{
\item \bold{Workspace Management - Copy}\cr
A copy of the selected object is obtained under a new name (requested by a dialogue box). 
To rename an object, copy it under a new name, and then delete the original object.
\item \bold{Workspace Management - Delete}\cr
By using this function the selected object is deleted from the workspace.
\item \bold{Workspace Management - Merge}\cr
A dialogue box asks the name of the object to be merged with the selected one. The result
 of this operation will be stored under the name of the selected object.
The following three cases are possible.\cr
\enumerate{
\item Same number of rows (r), different number of columns (c1 and c2). The merged object will have r 
rows and c1+c2 columns. The names of the rows will be same as those in the selected object.
\item Same number of columns (c), different number of rows (r1 and r2). The merged object will have 
r1+r2 rows and c columns. The names of the columns must be the same in the two objects. No duplicate 
name must be present in the r1+r2 rows.
\item Same number of rows (r) and same number of columns (c). Two different objects can be obtained, 
having 2r rows and c columns, or r rows and 2c columns, respectively. A dialogue box with the question
\emph{Merge Row-wise?} will appear. A positive answer will produce a merged object having 2r rows and 
c columns, while a negative answer will produce a merged object having r rows and 2c columns. In what 
concerns the names of the rows and the names of the columns, the same conditions/restrictions described 
in case 1) and 2) are applied.
}
Caution: the original selected object will be overwritten by the merged object.\cr
\item \bold{Workspace Management - Modify}\cr
This option allows the modification of an existing object from a spreadsheet window. The user can change
 the values of each single cell. To delete data just press on the \emph{Del} key, and \emph{NA} will appear.
 Right clicking on the spreadsheet causes the opening of a context-sensitive menu with new functionalities. 
 In this way the user can, for instance, change the type of data (e.g. from numeric to character), 
 sort the data, add, delete or rename columns and rows. In order to have a modification accepted the
 modified cells must be not highlighted any more. When pressing the \emph{OK} button the modifications 
 will be saved.\cr
\cr 
\figure{DHworkspacemanagement1.png}\cr
\cr
This function is intended only for minor changes. For more extensive changes it is suggested to modify the 
dataset by using an external editor such as a spreadsheet program, and load the file again in R after all 
the changes have been made.\cr
Caution: the original selected object will be overwritten by the modified object.\cr
\item \bold{Workspace Management - New}\cr
This function is used to create a new matrix. A dialogue box asks for the name of the matrix and its size 
(number of rows and columns).\cr
\cr 
\figure{DHworkspacemanagement2.png}\cr
\cr
Then a spreadsheet is shown where the user can type the data. Empty cells (missing values) are automatically
 replaced by \emph{NA}.\cr
\cr 
\figure{DHworkspacemanagement3.png}\cr
\cr
By right clicking on the name of a row (or of a column) it is possible to change the default value.
When pressing the \emph{OK} button the new matrix will be saved.
By placing the pointer on the first cell of the first row it is possible to perform copy/paste 
from an Excel file. In this case the number of rows and columns declared at the beginning can be 
smaller than the real size (not the other way round, otherwise the exceeding data will stay as 0). 
After having pasted the copied data, the following window appears\cr
\cr 
\figure{DHworkspacemanagement4.png}\cr
\cr
After clicking on the \emph{OK} button the spreadsheet for direct imputation will be shown again.
Caution: it is not possible to create matrices containing both numeric and alphanumeric variables
 (typical of the data.frame). \cr
\item \bold{Workspace Management - Tell Me}\cr
An editor window opens showing the following information about the selected object:
\itemize{
\item Name: the name of the object
\item Type: the type of the object according to R
\item Rows: the number of rows
\item Columns: the number of columns
\item NA: the number of Not Available numbers, i.e., missing data. In CSV and TXT files the missing 
data must be coded as NA, while in XLS/XLSX files the empty cells are directly interpreted as Not Available numbers
\item \%NA: the per cent of missing data on the total data
\item Row Names: the names of each row
\item Column names: the name of each column)
}
The editor window can be left open as long as it is necessary. Only one such window at a time can be opened.\cr
\cr 
\figure{DHworkspacemanagement5.png}\cr
\cr
\item \bold{Workspace Management - Transpose}\cr
A dialogue box asks the name under which the selected object will be stored after having been transposed 
(i.e., the original rows will be the columns and the original columns will be the rows).\cr
Caution: this function can only be applied on purely numeric data files, without row names and column 
names; in case of data sets containing missing data, alphanumerical data and/or row names and column
 names it is suggested to extract the non numerical part into separate vectors (see Data Set in this same menu)
 or to perform the transposition before loading the data file into R. 
}
}
\value{New objects are created in the memory depending of the user choice.}
\references{}
\author{Riccardo Leardi and Gianmarco Polotti with contributions from Giorgio Marubini.Gruppo di Chemiometria (Divisione di Chimica Analitica della Societa' Chimica Italiana)}
\note{}
\seealso{}
\examples{}
\keyword{ dataset }
