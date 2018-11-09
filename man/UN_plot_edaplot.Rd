\name{UN_plot_edaplot}
\alias{UN_plot_edaplot}
\title{Edaplot of a vector}
\description{Edaplot of a vector}
\usage{UN_plot_edaplot(previous.name = "")}
\arguments{
  \item{previous.name}{name of the default name in the menu}
}
\details{
\itemize{
\figure{UNplotmenu.png}
  \item{Vector}{name of the vector dataset}
}
\figure{UNploteda.png}
The data distribution plays an important role in statistics; 
chemometrics is not so happy with this concept because the number 
of available data is often small and the type of  distribution 
is often unknown. Actually, the value of a variable x 
(say the concentration of a chemical compound in a set of n
 samples) will have an \bold{Empirical Distribution}; whenever 
 possible the distribution of x should be visually inspected
 to obtain a better insight of the data. A number of different 
 plots can be used for this purpose.
\bold{Edaplot} stands for exploratory data analysis plot.  
It provides deeper insight into the univariate data distribution.
 The single groups are clearly visible in the one-dimensional 
 scatter plot; outliers are flagged by the boxplot and the form
 of the distribution is visualized by a histogram and a density 
 trace.
}
\value{A window plot}
\references{}
\author{Riccardo Leardi and Gianmarco Polotti with contributions from Giorgio Marubini.Gruppo di Chemiometria (Divisione di Chimica Analitica della Societa' Chimica Italiana)}
\note{}
\seealso{}
\examples{
A<-1:20
UN_plot_edaplot('A')
}
\keyword{ plot }
