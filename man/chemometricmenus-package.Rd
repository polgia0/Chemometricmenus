\name{chemometricmenus-package}
\alias{chemometricmenus-package}
\alias{chemometricmenus}
\docType{package}
\title{Chemometric Menus}
\description{Simplified chemometric method through graphical menus}
\details{
\tabular{ll}{
Package: \tab chemometricmenus\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-07-24\cr
License: \tab GPL-2\cr
}
\bold{Use of the main tool bar}\cr
\figure{general.png}\cr
The main control of the package is given by the Menu Bar as shown in the figure.
Each menus call several functions with their one graphical interface.
Caution:
\itemize{
	\item when a function is called the bar is minimized
	\item to magnify the bar again just click above
	\item to close the bar use the 'X' button at the top right corner
	\item \bold{close the bar submenu to get the control back to the console}
}
\bold{Input Variable Name}\cr
\figure{generalformula.png}\cr
This input box appears before each function is called and requires the name 
of the variable that is going to be processed. The name of the vector/matrix/dataframe
 must be typed considering that R is case sensitive. If more than one dataset is loaded,
 the default name selected is the last name that was input. So, as an example let the 
 matrix A be stored in the R memory and suppose that it must be processed.
The letter \emph{A} should be typed in the Input Variable Name box. It is possible to process
 its sub matrices using the R syntax. If \emph{A[,1]} is written then the first column 
 is processed. Please find here below some examples of names accepted in the Input Variable Box.
For more expert users: if the data set is a dataframe, the notation typical of dataframe
 fields, such as \emph{name_dataframe$name_field}, is also allowed.
Users must pay attention to respect the correct dimensions of the data input. Some functions 
apply only to vectors, other functions instead operate both on vectors and on matrices. 
Sometimes it is possible to load into R matrices and apply to them functions that process vectors; 
in this case the so-called parallel processing is carried out. E.g., if \emph{A[,1:3]} is typed
 in the input box calling the \emph{Univariate/Average/Arithmetic} function, the row vector  
 3 , 7.333 , 1.333 is displayed, where the three elements are the Arithmetic Average of the 
 columns 1, 2, and 3 of A, respectively.\cr
\bold{Outputs}\cr
The outputs of the required functions are displayed on the console. Moreover, results are 
also stored in the memory under are given a new variable name. A printout such as 
\emph{The value is saved in: var.mean} appears on the console before displaying the results. 
User can use results calling the new variable:
\enumerate{
\item in a following Input Box;
\item typing the new variable name (e.g. var.mean) on the console to display the results.
}
If results are graphics, they can be printed, saved and copied on the clipboard using 
the menu of the window where they are displayed.\cr
\bold{Note on Missing Data}\cr
In a table (i.e. a matrix or a dataframe) missing values are indicated by the symbol
 \emph{NA} which stands for \emph{Not Available}. Undetermined values such as 0/0 
 instead are represented by the symbol \emph{NaN} which stands for \emph{Not a Number}. 
When missing values are present in a dataset, any function in R applied to the dataset 
or on single vectors containing one or more NAs, will return NAs as a result, because 
any arithmetical operation with NA values will return NA values. However, in this menu 
only, all functions operate even though NAs are present. Missing values are ignored 
and calculations are performed without considering them. Together with the missing
 values, some other values may be removed automatically during computation. 
 In general, in a matrix or dataframe all the data are ignored in a row where one 
 or more NAs are present (row wise elimination) before running calculations by row 
 or by column as explained below. Only in the case when the matrix is indicated as 
 a whole, the single NA values are ignored. To make the user aware that the 
 calculation dealt with missing data this message appears on the console
 \emph{>>NA found and ignored<<}.
}
\author{Riccardo Leardi and Gianmarco Polotti with contributions from Giorgio Marubini.Gruppo di Chemiometria (Divisione di Chimica Analitica della Societa' Chimica Italiana)}
\references{
\itemize{
\item Varmuza K., Filzmoser P. (2009) \emph{Introduction to Multivariate Statistical Analysis in Chemometrics}, CRC Press,\  ISBN: 978-1-4200-5947-2
\item Iacus, S. M., Masarotto G. (2007) \emph{Laboratorio di statistica con R}, Ed.II, Italy: McGraw-Hill Companies,\ ISBN: 978-8-838-66369-7
\item Ildiko' F. F., Todeschini R. (1994) \emph{The Data Analysis Handbook}, the Netherlands: Elsevier Amsterdam
}
}
\keyword{ package }
\seealso{}
\examples{}
