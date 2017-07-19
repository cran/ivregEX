\name{sensOLS}
\alias{sensOLS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Sensitivity Interval for Least Squared Estimator
}
\description{
	This function performs the background sensitivity interval calculation for
	the OLS estimator in a regression model. 
}
\usage{
sensOLS(delta1, df, est, se, alpha = 0.05, talpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{delta1}{
	A numeric vector of values of the first sensitivity parameter. Default 
	value \code{seq(-.1, .1, .01)}.	Not used for 2SLS estimator.
}
  \item{df, est, se}{
	Degrees of freedom of the model; 2SLS estimate and its standard error.
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
	See the description of \code{\link{senslm}}.
}
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
\note{
Do not use this function by itself, use senslm function.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{senslm}}, \code{\link{plot.si}}
}
\keyword{ Sensitivity Analysis }
\keyword{ Sensitivity Interval }% __ONLY ONE__ keyword per line
