\name{ivreg.EX}
\alias{ivreg.EX}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Constructing independent evidence (EX estimates) in an IV regression
}
\description{
This function uses the same arguments as \code{\link[AER]{ivreg}} and additionally
outputs the EX analysis.
}
\usage{
ivreg.EX(formula, instruments, data, subset, na.action, weights, offset,
  contrasts = NULL, model = TRUE, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{formula, instruments}{formula specification(s) of the regression
    relationship and the instruments. Either \code{instruments} is missing and
    \code{formula} has three parts as in \code{y ~ x1 + x2 | z1 + z2 + z3}
    (recommended) or \code{formula} is \code{y ~ x1 + x2} and \code{instruments}
    is a one-sided formula \code{~ z1 + z2 + z3} (only for backward compatibility).}
  \item{data}{an optional data frame containing the variables in the model. 
    By default the variables are taken from the environment of the \code{formula}.}
  \item{subset}{an optional vector specifying a subset of observations to be used in 
    fitting the model.}
  \item{na.action}{a function that indicates what should happen when the 
    data contain \code{NA}s. The default is set by the \code{na.action} option.}
  \item{weights}{an optional vector of weights to be used in the fitting process.}
  \item{offset}{an optional offset that can be used to specify an a priori known
    component to be included during fitting.}       
  \item{contrasts}{an optional list. See the \code{contrasts.arg} of 
    \code{\link[stats:model.matrix]{model.matrix.default}}.}
  \item{model}{logicals.  If \code{TRUE} the corresponding components of
    the fit (the model frame, the model matrices , the response) are returned.}
  \item{\dots}{further arguments passed to \code{\link{ivreg.fit.EX}}.}
  
}
\details{
See Details on the arguments and others see \code{ivreg} of \code{AER} package.
Statistical analysis and sensitivity analysis based on the output of \code{ivreg.EX}
is performed in \code{coeftestIV} and \code{sensIvreg} functions.
}
\value{

\code{ivreg.EX} returns an object of class \code{"ivregEX"} (also of class 
\code{"ivreg"}), with the following components:
    \item{coefficients}{parameter estimates.}
    \item{residuals}{a vector of residuals.}
    \item{fitted.values}{a vector of predicted means.}
    \item{weights}{either the vector of weights used (if any) or \code{NULL} (if none).}
    \item{offset}{either the offset used (if any) or \code{NULL} (if none).}
    \item{n}{number of observations.}
    \item{nobs}{number of observations with non-zero weights.}
    \item{rank}{the numeric rank of the fitted linear model.}
    \item{df.residual}{residual degrees of freedom for fitted model.}
    \item{cov.unscaled}{unscaled covariance matrix for the coefficients.}
    \item{sigma}{residual standard error.}
    \item{call}{the original function call.}
    \item{formula}{the model formula.}
    \item{terms}{a list with elements \code{"regressors"} and \code{"instruments"}
      containing the terms objects for the respective components.}
    \item{levels}{levels of the categorical regressors.}
    \item{contrasts}{the contrasts used for categorical regressors.}
    \item{model}{the full model frame (if \code{model = TRUE}).}
    \item{y}{the response vector.}
    \item{x}{a list with elements \code{"regressors"}, \code{"instruments"}, \code{"projected"},
      containing the model matrices from the respective components.
	  \code{"projected"} is the matrix of regressors projected
      on the image of the instruments.}
}
%%\references{
%% ~put references to the literature/web site here ~
%%}

%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[AER]{ivreg}}, \code{\link{coeftestIV}}
}
\examples{

data("CigarettesSW", package = "AER")
CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)

fm2 <- ivreg.EX(log(packs) ~ log(rprice) + log(rincome) | tdiff + log(rincome), 
		data = CigarettesSW, subset = year == "1995")
lmfit <- lm(log(packs) ~ log(rprice) + log(rincome), data = CigarettesSW, subset = year == "1995")

coeftestIV(fm2, lmfit, 'log(rprice)')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ regression }
\keyword{ IV Model }% __ONLY ONE__ keyword per line
