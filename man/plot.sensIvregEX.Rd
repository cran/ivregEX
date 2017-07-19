\name{plot.sensIvregEX}
\alias{plot.sensIvregEX}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting of Sensitivity Analyses
}
\description{
	Based on the output of \code{sensIvreg} creates plots of sensitive regions.
}
\usage{
\S3method{plot}{sensIvregEX}(x, effect = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	An object of class 'sensIvregEX', output of \code{sensIvreg}.
}
  \item{effect}{
	Effect size (optional). Default value is 0.
}
  \item{\dots}{
	Optional arguments for R plotting features.
}
}

\author{
Bikram Karmakar
}
\note{
	This functionally creates two sets of plots, one a greyscale plot of the 
	sensitivity analyses in the secondary instrumental variables analysis 
	with independent EX analysis. Additionally it creates a plot of 
	individual sensitivity regions for each analyses for the given effect size.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{\link{plot.ivregDecision}}, 
}
\examples{

data("CigarettesSW", package = "AER")
CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)


fm2 <- ivreg.EX(log(packs) ~ log(rprice) + log(rincome) | tdiff + log(rincome), 
			data = CigarettesSW, subset = year == "1995")
lmfit <- lm(log(packs) ~ log(rprice) + log(rincome), data = CigarettesSW, subset = year == "1995")
res = coeftestIV(fm2, lmfit, 'log(rprice)')

z <- sensIvreg(fm2, res, 'log(rprice)', effect = c(-.1, 0, .1, .15, .17), show = FALSE)

plot(z, nc = 3)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Plotting }
\keyword{ Sensitivity Analysis }
\keyword{ Sensitivity Interval }
% __ONLY ONE__ keyword per line
