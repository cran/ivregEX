\name{plot_confregion}
\alias{plot_confregion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Confidence Region
}
\description{
	This function provides a visual representation of the confidence regions capturing 
	the three analyses, OLS, 2SLS and EX.
}
\usage{
	plot_confregion(est, se, t, roundto = 3, xlims, ylims = c(0, .7), ypos = .35, 
			xaxis = T, legend = T, xlab = 'Treatment Effect', xlab.cex = 1.3,
			oma = c(4.5, 0, 2, 0), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{est}{
	A numeric vector of length 3, for coeficient estimates of the three analyses - OLS,
	2SLS and EX respectively.
}
	\item{se}{
	A numeric vector of length 3, for standard errors of the three analyses - OLS,
	2SLS and EX respectively.
}
	\item{t}{
	A number to be used to calculate individual confidence interval by 'est \eqn{\pm} t*se'. Default 
	value used is 2. For different level of confidence corresponding value has to be provided.
	}
	\item{roundto}{
	The number of decimal digit precision.
	}
	\item{xlims}{
	Optional numeric array of size 2 for the plotting region of the treatment effect plotted.
	}
	\item{ylims, ypos}{
	Optional arguments. Can be used for plotting aeshetics.
	}
	\item{xaxis, legend}{
	Logical arguments for whether the treatment effect axis to be plotted and legends shown.
	}
	\item{xlab}{
	A logical argument or a character vecctor for the xaxis label.
	}
	\item{xlab.cex}{
	Size of xaxis label.}
	\item{oma}{
	Outer margin dimensions.
	}
	\item{\dots}{
	Other graphics parameters which are passed to the main plotting function.
	}
}

\author{
Bikram Karmakar
}
\note{
This function plots the confidence region of the treatment effect. The confidence region
is calculated from the coefficient estimates and standard errors of the OLS, 2SLS and EX.
This confidence region has overall level of confidence \eqn{(1-\alpha)\times 100\%} when each indivual 
analyses are all at the same level of significance. Argument \code{t} should be used to
specify the corresponding quantile.

Resultant confidence region has a four pieces: region where we have no evidence, region where 
only OLS shows evidence, region where OLS and one of other two etimators provides evidence, and
the region where both the IV based estimators show evidence.

This plotting function creates a new plotting device for its plotting purpose.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{


est = c(1.6858, 2.8912, 1.1435)
se = c(0.5029, 0.9785, 0.5377)

plot_confregion(est, se, roundto = 3, ypos = .35,  xlab.cex = 1.2)




}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Plotting }
\keyword{ Confidence Region }% __ONLY ONE__ keyword per line
