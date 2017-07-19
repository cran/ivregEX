\name{sens2SLS}
\alias{sens2SLS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Sensitivity Interval for Two Stage Least Squared Regression
}
\description{
	This function performs the background sensitivity interval calculation for
	the 2SLS estimator in an instrumental variables regression model. 
}
\usage{
sens2SLS(delta2, df, est, se, thatY, rho2, alpha = 0.05, talpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{delta2}{
	A numeric vector of values of the second sensitivity parameter. Default value \code{seq(-.1, .1, .01)}.
		Only used for 2SLS estimator.}
  \item{df, est, se}{
	Degrees of freedom of the model; 2SLS estimate and its standard error.
	}
  \item{thatY}{
	t-statistic of the treatment in the regression of the instruments on the 
	treatment.
	}
  \item{rho2}{
	squared partial correlation of g(instruments) and the treatment.
	}
  \item{alpha}{
	Significance level. Default at 0.05.
	}
  \item{talpha}{
	If the interval is to be adjested for model selection etc, the multiplicative 
	factor to be used. Optional.
}
}
\details{
See the description of \code{\link{sensIvreg}}.
}
\value{Returns an object of class "si".

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
\note{
Do not use this function by itself, use sensIvreg function.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sensIvreg}}, \code{\link{plot.si}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Sensitivity Analysis }
\keyword{ Sensitivity Interval }% __ONLY ONE__ keyword per line
