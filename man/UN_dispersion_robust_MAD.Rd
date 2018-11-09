\name{UN_dispersion_robust_MAD}
\alias{UN_dispersion_robust_MAD}
\title{MAD of a matrix}
\description{Median Absolute Deviation (MAD) of a matrix by rows or columns}
\usage{UN_dispersion_robust_MAD(previous.name = "")}
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
The \bold{Median Absolute Deviation} is a robust estimator of the 
standard deviation. \emph{R console notation: x_mad <- mad(x)}}
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
UN_dispersion_robust_MAD('A')
}
\keyword{ dispersion }
