\name{get_nrnc}
\alias{get_nrnc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find the grid structure in plotting
}
\description{
Takes input the number of plots and returns the argument of \code{mfrow}. Does 
not set \code{mfrow} in \code{par()}.
}
\usage{
get_nrnc(m, nc = -1, choices = 2:5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{
Number of plots, a positive integer.
}
  \item{nc}{
A preferred choice of number of columns. If set to -1, it is calculated 
based on \code{choices}.
}
  \item{choices}{
Various choices for \code{nc}.
}
}
\value{
Two numbers for nr and nc in \code{mfrow} argument of \code{par()}.
}

\author{
Bikram Karmakar
}
\note{
This is used in plotting results of sensitivity analyses, i.e., outputs of
\code{sensIvreg} or \code{senslm}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
\dontrun{
get_nrnc(6)
get_nrnc(5, 3)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Plotting }
