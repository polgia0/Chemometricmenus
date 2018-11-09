\name{UN_dispersion_RSD}
\alias{UN_dispersion_RSD}
\title{RSD dispersion of a matrix}
\description{Relative standard deviation of a matrix by rows or columns}
\usage{UN_dispersion_RSD(previous.name = "")}
\arguments{
  \item{previous.name}{name of the default name in the menu}
}
\details{
\figure{UNaveragemenu.png}
\itemize{
  \item{All}{calculation made on the whole matrix}
  \item{Row wise}{calculation made by rows}
  \item{Column wise}{calculation made by columns}
}
\figure{UNdispersionRSD.png}
The measures of spread provided by the standard deviation and variance 
are expressed in the same unit as the data. If data with different 
units ought to be compared or the spread is best represented in percent 
of the central value (as done in Analytical Chemistry to express the 
precision, for example) it is better to use a measure which is dimension 
free. Such a measure is the \bold{Coefficient of variation} or 
\bold{Realtive standard deviation(RSD)}. 
\emph{R console notation: x_rsd <- 100*sd(x)/mean(x)}
}
\value{
The output is saved in the variable \emph{var.dispersion}
  \item{All }{single numeric value}
  \item{Row wise}{numeric vector}
  \item{Column wise}{numeric vector}
}
\references{}
\author{Riccardo Leardi and Gianmarco Polotti with contributions from Giorgio Marubini.Gruppo di Chemiometria (Divisione di Chimica Analitica della Societa' Chimica Italiana)}
\note{}
\seealso{}
\examples{
A<-matrix(1:20,5,4)
UN_dispersion_RSD('A')
}
\keyword{ dispersion }
