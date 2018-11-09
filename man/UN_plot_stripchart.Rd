\name{UN_plot_stripchart}
\alias{UN_plot_stripchart}
\title{Strip chart of a vector}
\description{Strip chart plot of a vector}
\usage{UN_plot_stripchart(previous.name = "")}
\arguments{
  \item{previous.name}{name of the default name in the menu}
}
\details{
\itemize{
\figure{UNplotmenu.png}
  \item{Vector}{name of the vector dataset}
}
\figure{UNplotstripchart.png}
The data distribution plays an important role in statistics; 
chemometrics is not so happy with this concept because the number 
of available data is often small and the type of  distribution 
is often unknown. Actually, the value of a variable x 
(say the concentration of a chemical compound in a set of n
 samples) will have an \bold{Empirical Distribution}; whenever 
 possible the distribution of x should be visually inspected
 to obtain a better insight of the data. A number of different 
 plots can be used for this purpose.
A \bold{Strip Chart} plot or \bold{Strip Scatter} plot is a one 
dimensional scatter plot. It plots the data along a straight line; 
it is a handy tool for exploring small data sets which contain 
values not too close between them. 
}
\value{A window plot}
\references{}
\author{Riccardo Leardi and Gianmarco Polotti with contributions from Giorgio Marubini.Gruppo di Chemiometria (Divisione di Chimica Analitica della Societa' Chimica Italiana)}
\note{}
\seealso{}
\examples{
A<-1:20
UN_plot_stripchart('A')
}
\keyword{ plot }
