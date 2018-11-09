\name{UN_dispersion_robust_sMAD}
\alias{UN_dispersion_robust_sMAD}
\title{sMAD of a matrix}
\description{Median Absolute Deviation (MAD) of a matrix by rows or columns}
\usage{UN_dispersion_robust_sMAD(previous.name = "")}
\arguments{
  \item{previous.name}{name of the default name in the menu}
}
\details{
\figure{UNaveragemenu.png}
\itemize{
  \item{All}{calculation made on the whole matrix}
  \item{Row wise}{calculation made by rows}
}
\figure{UNdispersionrobustsmad.png}
MAD is based on the median taken as central value of the sample 
distribution; the absolute differences are computed and MAD is 
defined as the median of such differences. In the case of a 
normal distribution, MAD times 1.483 can be used for a robust
 estimation of the theoretical standard deviation.}
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
UN_dispersion_robust_sMAD('A')
}
\keyword{ dispersion }
