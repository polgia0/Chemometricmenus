\name{UN_average_arithmetic}
\alias{UN_average_arithmetic}
\title{Arithmetic mean average of a matrix}
\description{Arithmetic average of a matrix by rows or columns}
\usage{UN_average_arithmetic(previous.name = "")}
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
\figure{UNarithmeticaverageformula.png}
The average is the most used estimator for a central value in 
normal or approximately normal distributions, this is the 
\bold{Arithmetic Mean} ( \emph{mean(x)} in R notation). 
}
\value{
The output is saved in the variable \emph{var.mean}
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
UN_average_arithmetic('A')
}
\keyword{ average }
