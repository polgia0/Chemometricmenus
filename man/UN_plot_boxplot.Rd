\name{UN_plot_boxplot}
\alias{UN_plot_boxplot}
\title{Boxplot of a vector}
\description{Boxplot of a vector}
\usage{UN_plot_boxplot(previous.name = "")}
\arguments{
  \item{previous.name}{name of the default name in the menu}
}
\details{
\itemize{
\figure{UNplotmenu.png}
  \item{Vector}{name of the vector dataset}
}
\figure{UNplotbox.png}
The data distribution plays an important role in statistics; 
chemometrics is not so happy with this concept because the number 
of available data is often small and the type of  distribution 
is often unknown. Actually, the value of a variable x 
(say the concentration of a chemical compound in a set of n
 samples) will have an \bold{Empirical Distribution}; whenever 
 possible the distribution of x should be visually inspected
 to obtain a better insight of the data. A number of different 
 plots can be used for this purpose.
The bold{Boxplot} function is an informative graphics to display 
a data distribution, based on median and quartiles. According to
 Frank and Todeschini (1994) a boxplot can be defined as follows ):
 The height of the box is given by the first and third quartile, 
 and the mid line shows the median; the width of the box has 
 usually no meaning. One whisker extends from the first quartile 
 to the smallest data value in the interval Q1 to Q1-1.5IQR 
 and is called the lower whisker. The other whisker extends
 from the third quartile to the largest data value in the interval
 Q3 to Q3+1.5IQR and is called the upper whisker.
 Outliers,not within the range Q1-1.5IQR, Q3+1.5IQR, 
 are plotted as individual points.
}
\value{A window plot}
\references{}
\author{Riccardo Leardi and Gianmarco Polotti with contributions from Giorgio Marubini.Gruppo di Chemiometria (Divisione di Chimica Analitica della Societa' Chimica Italiana)}
\note{}
\seealso{}
\examples{
A<-1:20
UN_plot_boxplot('A')
}
\keyword{ plot }
