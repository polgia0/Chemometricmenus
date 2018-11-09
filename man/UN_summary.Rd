\name{UN_summary}
\alias{UN_summary}
\title{Summary table of a matrix}
\description{Principal statistic of a dataset}
\usage{UN_summary(previous.name = "")}
\arguments{
  \item{previous.name}{name of the default name in the menu}
}
\details{
\itemize{
\figure{UNaveragemenu.png}
  \item{previous.name}{name of the default name in the manu}
  \item{All}{calculation made on the whole matrix}
  \item{Row wise}{calculation made by rows}
  \item{Column wise}{calculation made by columns}
}
 \figure{UNsummary.png}
If a vector is given, the function \bold{Summary} returns a 6 numbers
 descriptive statistics of the data, namely minimum, maximum value,
 Mean, Median, 1st and  3rd quartiles, as shown below for 
 a matrix A (10,7).}
\value{A table of main statistic as in the figure}
\references{}
\author{Riccardo Leardi and Gianmarco Polotti with contributions from Giorgio Marubini.Gruppo di Chemiometria (Divisione di Chimica Analitica della Societa' Chimica Italiana)}
\note{}
\seealso{}
\examples{
A<-matrix(1:20,5,4)
UN_summary('A')
}
\keyword{ summary }
