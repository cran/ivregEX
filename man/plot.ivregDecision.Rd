\name{plot.ivregDecision}
\alias{plot.ivregDecision}
\alias{plot.ivregDecisionList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting an object of class 'ivregDecision' or 'ivregDecisionList'
}
\description{
	An object of class 'ivregDecisionList', which is a list of objects of class
	'ivregDecision', is output of 'sensIvreg.ivregEX'. This 
	function gives you freedom to stylize a gray-scale plot of a sensitivity
	analysis.
}
\usage{
\S3method{plot}{ivregDecision}(x, xaxes = TRUE, yaxes = TRUE, xaxes.label = TRUE, 
		yaxes.label = TRUE, legend = TRUE, ...)
		
\S3method{plot}{ivregDecisionList}(x, xaxes = TRUE, yaxes = TRUE, xaxes.label = TRUE, 
		yaxes.label = TRUE, legend = TRUE, subtitle =  TRUE, nc, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	An object of class 'ivregDecision' or 'ivregDecisionList'. A decision matrix
	(or list of matrices) for given treatment effect(s) for different values
	of the sensitivity parameters.
}
  \item{xaxes, yaxes, xaxes.label, yaxes.label}{
	Whether x-axis, y-axis and their labels to be added to the plots.
}
  \item{legend}{
	whether a legend will be added to the plot.
}
	\item{subtitle}{Logical for whether a subtitle to be added in a plot indicating 
		the effect size of a 'ivregDecisionList' object.
	}
 \item{nc}{
	Optional integer for the number of columns in the plot.
}
  \item{\dots}{
	Optional arguments for R plotting function.
}
}

\author{
Bikram Karmakar
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plot.sensIvregEX}}
}
\examples{
data("CigarettesSW", package = "AER")
CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)


fm2 <- ivreg.EX(log(packs) ~ log(rprice) + log(rincome) | tdiff + log(rincome), 
		data = CigarettesSW, subset = year == "1995")
lmfit <- lm(log(packs) ~ log(rprice) + log(rincome), data = CigarettesSW, 
		subset = year == "1995")
res = coeftestIV(fm2, lmfit, 'log(rprice)')

z <- sensIvreg(fm2, res, 'log(rprice)', delta1 = seq(-.51, .51, .01), 
		effect = c(-.1, 0, .1, .15, .17))

plot(z$Z[[1]])
plot(z$Z, nc = 3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Plotting }
\keyword{ Sensitivity Analysis }
% __ONLY ONE__ keyword per line
