\name{UN_plot_Y}
\alias{UN_plot_Y}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot of a vector}
\description{Plot of a vector with a line or some points}
\usage{UN_plot_Y(previous.name = "")}
\arguments{
  \item{previous.name}{name of the default name in the menu}
}
\details{
\figure{UNplotmenuY.png}
\itemize{
  \item{Vector Y}{name of the vector dataset}
  \item{Label Vector}{vector name of the vector elements}
  \item{Color Vector}{vector color of the vector elements}
  \item{X label}{label on the X axis}
  \item{Y label}{label on the Y axis}
  \item{Plot title}{label on the plot title}
  \item{Dataset names}{use datset row names}
  \item{Lines}{draw elements with a single line}
  \item{Points}{draw elements with points}
}
\figure{UNplotY.png}
The data distribution plays an important role in statistics; 
chemometrics is not so happy with this concept because the number 
of available data is often small and the type of  distribution 
is often unknown. Actually, the value of a variable x 
(say the concentration of a chemical compound in a set of n
 samples) will have an \bold{Empirical Distribution}; whenever 
 possible the distribution of x should be visually inspected
 to obtain a better insight of the data. A number of different 
 plots can be used for this purpose.
 This function calls a plot a vector Y vs. its index. 
 The \bold{Y plot} function calls a dialogue box in which the
 User I required to in out the Y vector that he wants to plot,
 along with a Label Vector (same length of Y) with the 
 alpha-numeric values that will be drawn in place of each 
 single element of Y. It is also possible to input a Color Vector 
 (which must have the same length of Y) with the factor level 
 of each element. With this choice a color is used for each
 different group (scale red-black-green). Other possible input
 are : plot title,labels for X and Y axes and the choice of drawing points, 
 lines or both. Example of different outputs are shown. 
}
\value{A window plot}
\references{}
\author{Riccardo Leardi and Gianmarco Polotti with contributions from Giorgio Marubini.Gruppo di Chemiometria (Divisione di Chimica Analitica della Societa' Chimica Italiana)}
\note{}
\seealso{}
\examples{
A<-1:20
UN_plot_Y('A')
}
\keyword{ plot }
