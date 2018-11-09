\name{DH_load_txt}
\alias{DH_load_txt}
\title{Load a TXT file}
\description{Load files made by TXT standard}
\usage{DH_load_txt(previous.name = "")}
\arguments{
  \item{previous.name}{Default name for the new dataframe, optional}
}
\details{
The \bold{Load} option allows the user to load a file into the workspace.
The following sub-options are present:
\itemize{
 \item CSV
 \item TXT
 \item XLS/XLSX
}
Note: in all the dialogue boxes the fields marked by \emph{*} must be filled, 
while the fields without the \emph{*} can be left blank.
Cautions:
\enumerate{
\item for undetected reason it is not possible to load files having more than 700 columns. 
    If such a file has to be loaded, the following two options can be used:
	\enumerate{
	\item transpose the original data file, load it and then transpose it again (see Workspace Management)
	\item split the original data files into blocks of acceptable size, load them and then merge
	them (see Workspace Management)
	}
\item if a column contains both numeric and alphanumeric values, this column will be 
 considered as alphanumeric and therefore no numeric computations will be possible on it
 (not even if only numerical values are selected); this characteristic will be kept 
 also in case of modifications removing the alphanumeric data. 
}
This option is used to import data from a TXT file. After the selection of the file 
(see Load/CSV), the following dialogue box is shown.\cr
\figure{DHloadtxtmenu.png}\cr
The default name proposed to the user is the name of the file (if blank characters are present, they are removed).
The default settings for decimal separator is \emph{.}.
\emph{Skip Top Rows} requires the input of an integer number n, and allows to skip the reading of the first n rows.
\emph{Skip Left Columns}, requires the input of an integer number m and allows to skip the reading of the first m columns.
If the first row of the file contains the names of the columns, the \emph{Header} box should be selected (option given by default).
If not, the names of the columns will be \emph{V1, V2, V3, ...}.
If the first column of the file contains the names of the rows, the \emph{Row Names} box should be selected (note: in order
 to be selected as \emph{Row Names}, the column must not contain duplicated values). If not, the names of the rows will be 
 \emph{1, 2, 3, ...}.Finally, if the fields are separated by a Tab (instead of a blank) the \emph{Tab Separator} box must be checked.\cr
After pressing the \emph{OK} button a check is made to verify whether the same name 
is already present in the workspace. If so, then the warning message in the Figure 
below appears informing the user that if he proceeds the variable to be loaded will 
overwrite the previous one.\cr
\figure{DHloadynmenu.png}\cr
An example of a TXT file that can be read by this option is reported.\cr
\tabular{rll}{
G \tab	R \tab	T\cr
a1 \tab	18	\tab 512\cr
a2 \tab 23	\tab 514\cr
a3 \tab 36	\tab 81.45\cr
a4	\tab 41.85 \tab 89\cr
a5	\tab 52.043	\tab 67\cr
}
After the file has been successfully loaded, the following message will be displayed:
\emph{[1] xx Data loaded: r Rows & c Columns}\cr
Note: if the \emph{Header} and \emph{Row Names} options have been activated, 
the corresponding row and column will be treated separately and will not be 
taken into account as row and column of the data matrix. As an example, after
 having loaded the previous data file (5 rows and 2 columns), the displayed 
 message will be:\cr
\emph{[1] 10 Data loaded: 5 Rows & 2 Columns}\cr
}
\value{A new data.frame is added into the memory}
\references{}
\author{Riccardo Leardi and Gianmarco Polotti with contributions from Giorgio Marubini.Gruppo di Chemiometria (Divisione di Chimica Analitica della Societa' Chimica Italiana)}
\note{}
\seealso{}
\examples{}
\keyword{ load }
