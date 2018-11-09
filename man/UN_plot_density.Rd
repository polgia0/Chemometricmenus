\name{UN_plot_density}
\alias{UN_plot_density}
\title{Density plot of a vector}
\description{Density plot of a vector}
\usage{UN_plot_density(previous.name = "")}
\arguments{
  \item{previous.name}{name of the default name in the menu}
}
\details{
\itemize{
\figure{UNplotmenu.png}
  \item{Vector}{name of the vector dataset}
}
\figure{UNplotdensity.png}
The data distribution plays an important role in statistics; 
chemometrics is not so happy with this concept because the number 
of available data is often small and the type of  distribution 
is often unknown. Actually, the value of a variable x 
(say the concentration of a chemical compound in a set of n
 samples) will have an \bold{Empirical Distribution}; whenever 
 possible the distribution of x should be visually inspected
 to obtain a better insight of the data. A number of different 
 plots can be used for this purpose.
This function calls the \bold{Probability density function/trace (PDF)},
 which is a smoothed line obtained using the top of the histogram 
 boxes as points. The smoothing parameters influence the appearance
 of the curve; the ordinate is scaled to give an area of 1 under 
 the curve.
}
\value{A window plot}
\references{}
\author{Riccardo Leardi and Gianmarco Polotti with contributions from Giorgio Marubini.Gruppo di Chemiometria (Divisione di Chimica Analitica della Societa' Chimica Italiana)}
\note{}
\seealso{}
\examples{
A<-1:20
UN_plot_density('A')
}
\keyword{ plot }
