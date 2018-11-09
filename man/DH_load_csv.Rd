\name{DH_load_csv}
\alias{DH_load_csv}
\title{Load a CSV file}
\description{Load files made by CSV standard}
\usage{DH_load_csv(previous.name = "")}
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
This option is used to import data from a CSV file. A first directory box allows to select the data file.\cr
\figure{DHloadcsv.png}\cr
A second box allows to type the name of the variable and asks for the format of the data file.\cr
\figure{DHloadcsvmenu.png}\cr
The default name proposed to the user is the name of the file (if blank characters are present, they are removed). 
The default settings are \emph{;} as field separator, \emph{.} as decimal separator, \emph{NA} for missing value.
\emph{Skip Top Rows} requires the input of an integer number n, and allows to skip the reading of the first n rows.
\emph{Skip Left Columns}, requires the input of an integer number m and allows to skip the reading of the first m columns.
If the first row of the file contains the names of the columns, the \emph{Header} box should be selected (option given by default).
If not, the names of the columns will be \emph{V1,V2,V3,...}\cr
If the first column of the file contains the names of the rows, the \emph{Row Names} box should be selected 
(note: in order to be selected as \emph{Row Names}, the column must not contain duplicated values). 
If not, the names of the rows will be \emph{1,2,3...}\cr
After pressing the \emph{OK} button a check is made to verify whether the same name is already present 
in the workspace. If so, then the warning message in the Figure below appears informing the user that if he 
proceeds the variable to be loaded will overwrite the previous one.\cr
\figure{DHloadynmenu.png}\cr
An example of a file that can be read by this option is reported. In it the first row is the header, 
with the names of the fields; the first field is the name of the sample and fields 2-11 contain the data. 
As can be seen, apart from the name, the file can contain also alphanumerical variables, as in field 10 (\emph{appearance})\cr
\cr
\emph{ 
batch;dry;pH;viscosity;nco;strength;elongation;modulus;shore;appearance;overall\cr
B1;36.76;7.65;80;4.91;34.36;477.32;22.55;33;ok;3\cr
B2;36.99;8.03;80;NA;33.95;459.84;92.94;35;ok;3\cr
B3;36.89;8.03;70;4.82;40.98;462.65;30.54;42;ok;1\cr
B4;36.88;7.69;100;4.86;26.27;414.69;65.65;31;ok;4\cr
B5;36.85;7.69;124;4.86;26.27;414.69;65.65;31;ok;4\cr
B6;37.14;8.02;127;4.55;37.35;471.96;74.67;40;ok;2\cr
B7;37.5;7.77;158;4.55;37.35;471.96;74.67;40;ok;2\cr
B8;37.37;7.83;148;NA;0;498.12;NA;NA;no;5\cr
B9;36.86;8.2;118;NA;0;502.15;NA;NA;no;5\cr
B10;36.89;8.18;130;4.39;22.04;454.74;32.37;42;ok;4\cr
B11;36.97;7.9;97;4.63;33.17;493.98;62.32;43;ok;3\cr
B12;37.19;8.24;128;NA;0;495.07;NA;NA;no;5\cr
B13;37.39;8.23;107;5.29;25.89;397.31;134.39;42;ok;4\cr
B14;37.16;8.68;106;4.56;15.8;191.01;132.52;40;no;5\cr
B15;37.21;7.46;77;NA;0;505.32;NA;NA;no;5\cr
B16;36.9;7.41;89;4.5;29.5;407.97;29.66;33;ok;4\cr
B17;37.06;8.06;99;4.53;24.77;370.66;78.68;42;ok;4\cr
B18;37.28;7.32;85;NA;0;487.98;NA;NA;no;5\cr
B19;37.16;7.53;123;4.94;28.49;370.59;243.52;43;ok;4\cr
B20;37.24;7.6;100;4.55;35.2;414.67;24.06;34;ok;3\cr
B21;37.57;7.57;89;4.71;26.048;376.878;56.369;36;ok;4\cr
}
\cr
After the file has been successfully loaded, the following message will be displayed:
\emph{[1] xx Data loaded: r Rows & c Columns}\cr
Note: if the \emph{Header} and \emph{Row Names} options have been activated, the corresponding 
row and column will be treated separately and will not be taken into account as row and 
column of the data matrix. As an example, after having loaded the previous data file
 (22 rows and 11 columns), the displayed message will be:\cr
\emph{[1] 242 Data loaded: 21 Rows & 10 Columns}\cr
}
\value{A new data.frame is added into the memory}
\references{}
\author{Riccardo Leardi and Gianmarco Polotti with contributions from Giorgio Marubini.Gruppo di Chemiometria (Divisione di Chimica Analitica della Societa' Chimica Italiana)}
\note{}
\seealso{}
\examples{}
\keyword{ load }
