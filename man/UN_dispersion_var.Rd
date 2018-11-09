\name{UN_dispersion_var}
\alias{UN_dispersion_var}
\title{Variance of a matrix}
\description{Variance of a matrix by rows or columns}
\usage{UN_dispersion_var(previous.name = "")}
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
\figure{UNdispersionvariance.png}
In statistics it is useful to work with the concept of squared 
deviations from a central value; the average of the squared deviations 
of the values from the mean is the Variance. Consequently, 
the unit of measure of the variance is the squared data unit of the values.
The estimator of the variance is the \bold{Sample Variance} 
(\emph{var(x) in R notation}), defined as the squared standard deviation.
\emph{R console syntax: x_var <- var(x)}
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
UN_dispersion_var('A')
}
\keyword{ dispersion }
