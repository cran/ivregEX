\name{sensEX}
\alias{sensEX}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Sensitivity Interval for Secondary EX Estimator
}
\description{
	This function performs the background sensitivity interval calculation for
	the EX estimator in an instrumental variables regression model. 
	}
\usage{
sensEX(delta1, df, est, se, rhoYX1.X1sq, alpha = 0.05, talpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{delta1}{
  A numeric vector of values of the first sensitivity parameter. Default value \code{seq(-.1, .1, .01)}.
		Not used for 2SLS estimator.}
  \item{df, est, se}{
	Degrees of freedom of the model; 2SLS estimate and its standard error.
	}
  \item{rhoYX1.X1sq}{
	squared partial correlation of the instruments and the treatment.
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

\keyword{ Sensitivity Analysis }
\keyword{ Sensitivity Interval }% __ONLY ONE__ keyword per line
