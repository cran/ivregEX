\name{senslm}
\alias{senslm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Sensitivity analysis of a least squared regression estimator
}
\description{
	Performs a sensitivity analysis of the least squared regression 
}
\usage{
senslm(lmfit, coefmat, variable, delta1 = seq(-0.1, 0.1, 0.01), alpha = 0.05, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	  \item{lmfit}{
		An obeject of class "lm".
	}
	  \item{coefmat}{
		Optional matrix of class "coeftest" an output of \code{\link[lmtest]{coeftest}}.
	}
	  \item{variable}{
		Treatment variable of interest.
	}
	  \item{delta1}{
		A numeric vector of values of the first sensitivity parameter. Default value \code{seq(-.1, .1, .01)}.
	}
	\item{alpha}{
		Significance level. Default at 0.05.
	}
	  \item{\dots}{
	 May be ignored, used in \code{\link{sensOLS}}.
	}
}
\details{
	delta1 is the partial 
	correlation of the unobservable and the treatment. It measures the amount
	of violation in the assumption of no-unmeasured confounding.}
\value{
	Returns an object of class "si".

	An object if class "si" is essentially a matrix of two columns for the 
	the upper and lower sensitivity limits corresponding to the sensitivity 
	parameters. 
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Bikram Karmakar
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sensIvreg}}, \code{\link{plot.sensIvregEX}}, \code{\link{plot.si}}
}
\examples{
	
data("CigarettesSW", package = "AER")
CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)

lmfit <- lm(log(packs) ~ log(rprice) + log(rincome), data = CigarettesSW, subset = year == "1995")

z <- senslm(lmfit, variable = 'log(rprice)')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ regression }
\keyword{ Sensitivity Analysis }
\keyword{ Sensitivity Interval }% __ONLY ONE__ keyword per line
