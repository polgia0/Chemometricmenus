\name{UN_plot_hist}
\alias{UN_plot_hist}
\title{Histogram of a vector}
\description{Histogram plot of a vector}
\usage{UN_plot_hist(previous.name = "")}
\arguments{
  \item{previous.name}{name of the default name in the menu}
}
\details{
\itemize{
\figure{UNplotmenu.png}
  \item{Vector}{name of the vector dataset}
}
\figure{UNplothistogram.png}
The data distribution plays an important role in statistics; 
chemometrics is not so happy with this concept because the number 
of available data is often small and the type of  distribution 
is often unknown. Actually, the value of a variable x 
(say the concentration of a chemical compound in a set of n
 samples) will have an \bold{Empirical Distribution}; whenever 
 possible the distribution of x should be visually inspected
 to obtain a better insight of the data. A number of different 
 plots can be used for this purpose.
The \bold{Histogram} is the most frequently used graphic for 
frequency plots. Histograms allow easily to recognize the shape
 of the empirical distribution of x; however, outliers may cause
 problems.
The first step in the construction of a histogram is the
 selection of the number of class intervals, also called classes,
 or bins, in which the data set values should be grouped. 
 As regards the choice of the number of class intervals,
 several rules of thumb are applied such as for instance the 
 square root of n. The subject however is not trivial, and the
 User is addressed to Reference books quoted or to R forum sites
 such as \emph{http://stackoverflow.com/}, and
 \emph{http://www.statmethods.net/} or similar searching the keywords 
 \emph{bin width} or \emph{Number of bins}.
}
\value{A window plot}
\references{}
\author{Riccardo Leardi and Gianmarco Polotti with contributions from Giorgio Marubini.Gruppo di Chemiometria (Divisione di Chimica Analitica della Societa' Chimica Italiana)}
\note{}
\seealso{}
\examples{
A<-1:20
UN_plot_hist('A')
}
\keyword{ plot }
