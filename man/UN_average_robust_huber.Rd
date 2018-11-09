\name{UN_average_robust_huber}
\alias{UN_average_robust_huber}
\title{Huber average of a matrix}
\description{Huber average of a matrix by rows or columns }
\usage{UN_average_robust_huber(previous.name = "")}
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
The \bold{Huber} is a robust estimator of the average}
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
UN_average_robust_huber('A')
}
\keyword{ average }
