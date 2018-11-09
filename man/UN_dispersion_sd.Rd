\name{UN_dispersion_sd}
\alias{UN_dispersion_sd}
\title{Standard deviation of a matrix}
\description{Standard deviation of a matrix by rows or columns}
\usage{UN_dispersion_sd(previous.name = "")}
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
 \figure{UNdispersionstandarddeviation.png}
 The most used measure for the spread of data is the 
\bold{Standard Deviation}(\emph{sd(x) in R notation}) which should
 not be mixed up with the theoretical standard deviation sigma of
 the normal distribution.
 The standard deviation is very sensitive to outliers; if the 
 distribution of the values is skewed, not only the mean will be biased,
 but also the standard deviation of the sample, s, will be even more
 biased since the standard deviation, by definition, is the sum of the
 squared deviations from the mean. The standard deviation is expressed 
 using the same units of the data. In the case of normal or approximately
 normal distributions, s is the best measure of spread because it is the
 most precise estimator for sigma, the population standard deviation; 
 unfortunately in practice the standard deviation is often uncritically 
 used instead of taking into consideration other more robust measures 
 for the spread of the sample values (see below Robust estimators in the
 Dispersion section).
\emph{R console syntax: x_sd <- sd(x)}
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
UN_dispersion_sd('A')
}
\keyword{ dispersion }
