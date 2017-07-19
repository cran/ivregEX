\name{print.si}
\alias{print.si}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Printing object of class "si"
}
\description{
	Printing feature of sensitivity intervals. Object of class "si" is found 
	in output of functions \code{sensIvreg} and \code{senslm}.
}
\usage{
\S3method{print}{si}(x, effect = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	object of class "si".
}
  \item{effect}{
	Null treatment effect.
}
	\item{\dots}{Ignore.}
}
\author{
Bikram Karmakar
}

\seealso{
\code{\link{plot.si}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Sensitivity Analysis }
\keyword{ Sensitivity Interval }% __ONLY ONE__ keyword per line
