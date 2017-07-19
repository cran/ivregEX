\name{plot.si}
\alias{plot.si}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Insensitive Region
}
\description{
	This function provides a visual representation of the sensitivity intervals.
}
\usage{
\S3method{plot}{si}(x, effect = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	An object of class 'si'; output of \code{senslm} and \code{sensIvreg.ivreg}.
}
  \item{effect}{
	Optional value of the null treatment effect.
}
  \item{\dots}{
	Optional arguments for the R plotting function.
}
}

\author{
Bikram Karmakar
}
\note{
This function is called as a part of plotting object of class 'sensIvregEX'.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{

data("CigarettesSW", package = "AER")
CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)

library(AER)


lmfit <- lm(log(packs) ~ log(rprice) + log(rincome), data = CigarettesSW, subset = year == "1995")

z <- senslm(lmfit, variable = 'log(rprice)')
plot(z)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Plotting }
\keyword{ Sensitivity Interval }% __ONLY ONE__ keyword per line
