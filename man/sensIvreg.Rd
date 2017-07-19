\name{sensIvreg}
\alias{sensIvreg}
\alias{sensIvreg.ivregEX}
\alias{sensIvreg.ivreg}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sensitivity analysis in instrumental variables regression model
}
\description{
	This function performs sensitivity analysis for two stage least squared 
	estimator. When output of OLS and EX analyses are also supplied as an
	object of class "ivregEX" all three sensitivity analyses for the three
	estimates are performed.
}
\usage{
\S3method{sensIvreg}{ivregEX}(ivregfit, coeftestIVres, variable, 
			delta1 = seq(-0.1, 0.1, 0.01), delta2 = seq(-0.1, 0.1, 0.01), 
			effect = 0, g = sqrt, errType = c("homoscedastic", 
			"heteroscedastic"), alpha = 0.05, show = FALSE, ...)
			
\S3method{sensIvreg}{ivreg}(ivregfit, coeftestIVres, variable, 
			delta2 = seq(-.1, .1, .01), 
			g = sqrt, errType = c("homoscedastic", "heteroscedastic"),
			alpha = 0.05, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{ivregfit}{
	An object of class \code{ivreg} or \code{ivregEX} respectively. \code{ivregEX} object is an 
	output of \code{\link{ivreg.EX}} or \code{\link{ivreg.fit.EX}}. \code{ivreg}
	object is an output of \code{\link[AER]{ivreg}}.
	}
	  \item{coeftestIVres}{
		An object of class "coeftest". Must be supplied if \code{ivregfitEX} 
		as the output of \code{coeftestIV}.	Optional for only a two stage least squared estimator.
	}
	 \item{variable}{
		The endogenous variable of interest in the instrumental variables regression 
		model.
	}
	  
		
	\item{delta1}{
		A numeric vector of values of the first sensitivity parameter. Default value \code{seq(-.1, .1, .01)}.
		Not used for 2SLS estimator.
	}
	  \item{delta2}{
		A numeric vector of values of the second sensitivity parameter. Default value \code{seq(-.1, .1, .01)}.
		Only used for 2SLS estimator.
	}
	  \item{effect}{
		A numeric vector of null treat effects of interest. Default 0.
	}
	  \item{g}{
		A function describing the mechanism by which the assumptions of 
		an instrument may be violated. Default sqrt.
	}
	  \item{errType}{
		What kind of error structure to be assumed: "homoscedastic" or "heteroscedastic".
	}
	  \item{alpha}{
		Significance level. Default at 0.05.
	}
	  \item{show}{
		Whether result is to be printed.
	}
	  \item{\dots}{
		Optional. May be ignored. used in \code{\link{sensOLS}}, \code{\link{sens2SLS}} 
			or \code{\link{sensEX}}
	}
}
\details{
	delta1 and delta2 are the two sensitivity parameters. delta1 is the partial 
	correlation of the unobservable and the treatment. It measures the amount
	of violation in the assumption of no-unmeasured confounding. delta2 is 
	the partial correlation of the outcome and g(instruments) measuring the 
	amount of violation in the exogeneity and no direct effect assumption of 
	the instruments.
	
	Only the 2SLS analysis uses delta2, OLS and EX analysis uses delta1 to 
	perform sensitivity analysis.
}
\value{
	Returns an  object of class "sensIvregEX" if input is of type \code{ivregEX}.
	If the input is of type \code{ivreg}, returns an object of class "si".

	An object if class "si" is essentially a matrix of two columns for the 
	the upper and lower sensitivity limits corresponding to the sensitivity 
	parameters. 

	An object of class "sensIvregEX" consists of an object of class "ivregDecisionList"
	and three objects of class "si" for the three estimators. "ivregDecisionList"
	is a list of overall sensitivity analyses (based on primary OLS and 
	secondary 2SLS and EX analysis. Each list element correspond to the deferent
	\code{effect} values and tells you the sensitivity of the problem 
	for all possible pairs of (delta1, delta2).
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Bikram Karmakar
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{senslm}}, \code{\link{plot.sensIvregEX}}, \code{\link{plot.si}}
}
\examples{
data("CigarettesSW", package = "AER")
CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)


######################
library(AER)

fm1 <- ivreg(log(packs) ~ log(rprice) + log(rincome) | tdiff + log(rincome),
		data = CigarettesSW, subset = year == "1995", x=TRUE)

z <- sensIvreg(fm1, variable = 'log(rprice)')


######################

fm2 <- ivreg.EX(log(packs) ~ log(rprice) + log(rincome) | tdiff + log(rincome), 
		data = CigarettesSW, subset = year == "1995")
lmfit <- lm(log(packs) ~ log(rprice) + log(rincome), data = CigarettesSW, 
		subset = year == "1995")
res = coeftestIV(fm2, lmfit, 'log(rprice)')

z <- sensIvreg(fm2, res, 'log(rprice)', effect = c(-.1, 0, .1, .15, .17))

###################

x = CigarettesSW[CigarettesSW[,'year'] == "1995", c('rprice', 'rincome')]
y = CigarettesSW[CigarettesSW[,'year'] == "1995", c('packs')]
z = CigarettesSW[CigarettesSW[,'year'] == "1995", c('tdiff', 'rincome')]

fm3 <- ivreg.fit.EX(as.matrix(x), y, z)
lmfit <- lm(packs ~ rprice + rincome, data = CigarettesSW, subset = year == "1995")
res = coeftestIV(fm3, lmfit, 'rprice')
z <- sensIvreg(fm3, res, 'rprice', effect = c(-.1, 0, .1, .15, .17))

##################

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Sensitivity Analysis }
\keyword{ Sensitivity Interval }% __ONLY ONE__ keyword per line
\keyword{ IV Model }
