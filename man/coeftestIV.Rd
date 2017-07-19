\name{coeftestIV}
\alias{coeftestIV}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Coefficient test of output of ivreg.EX.
}
\description{
For a given set of endogenous variables in an instrumental variables model 
coeftestIV provides you the coefficients matrix with estimates, std errs, 
t-ratios and p-values for both homoscedastic and heteroscedastic errors.
Also, clustered standard error can be calculated.
}
\usage{
coeftestIV(ivregfit, lmfit, vars, cls = NULL, vcov)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ivregfit}{
An object of class 'ivregEX', an output of ivreg.EX or ivreg.fit.Ex function.
}
  \item{lmfit}{
An object of class 'lm', an output of lmfit or lm function.
}
  \item{vars}{
A vector of endogenous variables of the instrumental variables model for which
the standard error calculation and significance test to be performed. 
}
  \item{cls}{
A numeric vector specifying the stratification strata structure of the model. 
Default setting of NULL assumes no stratification.
}
	\item{vcov}{
	A specification of the covariance matrix of the estimated coefficients. This
	can be specified as a matrix or as a function yielding a matrix when applied to x.
	Only used for the computation under heteroscedastic errors. Default is vcovHC.
}
}
\details{
For details on \code{vcov} see the documentation of \code{coeftest} in \code{"lmtest"}
package. 
}
\value{
An object of class \code{'coeftest'}. A matrix of coefficient estimates, standard errors,
t-ratios and p-values.
}

\author{
Bikram Karmakar
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[lmtest]{coeftest}}
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

##############
x = CigarettesSW[CigarettesSW[,'year'] == "1995", c('rprice', 'rincome')]
y = CigarettesSW[CigarettesSW[,'year'] == "1995", c('packs')]
z = CigarettesSW[CigarettesSW[,'year'] == "1995", c('tdiff', 'rincome')]

fm3 <- ivreg.fit.EX(as.matrix(x), y, z)
lmfit <- lm(packs ~ rprice + rincome, data = CigarettesSW, subset = year == "1995")
res = coeftestIV(fm3, lmfit, 'rprice')
res

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
\keyword{ sandwich method }
