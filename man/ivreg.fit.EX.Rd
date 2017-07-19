\name{ivreg.fit.EX}
\alias{ivreg.fit.EX}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Fits the Two Stage Least Squared Estimates and EX estimates 
}
\description{
	Calculates the instrumental variables model when the output, regressors and 
	instrument matrices are explicitly provided.  
}
\usage{
ivreg.fit.EX(x, y, z, weights, offset, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{regressor matrix.}
  \item{y}{vector with dependent variable.}
  \item{z}{instruments matrix.}
  \item{weights}{an optional vector of weights to be used in the fitting process.}
  \item{offset}{an optional offset that can be used to specify an a priori known
    component to be included during fitting.}       
  \item{\dots}{further arguments passed to \code{\link[stats:lmfit]{lm.fit}} or
    \code{link[stats]{lm.wfit}}, respectively.}
}
\details{
	See documentation of \code{\link[AER]{ivreg.fit}} for further operational 
	details. 
	
	Statistical inference and sensitivity analysis based on the output of 
	\code{ivreg.fit.Ex} can be performed using \code{coeftestIV} and \code{sensIvreg}
	functions.
}
\value{ 
\code{ivreg.fit.EX} returns an object of class \code{ivregEX} list with the following components:
    \item{coefficients}{parameter estimates.}
    \item{residuals}{a vector of residuals.}
    \item{fitted.values}{a vector of predicted means.}
    \item{weights}{either the vector of weights used (if any) or \code{NULL} (if none).}
    \item{offset}{either the offset used (if any) or \code{NULL} (if none).}
    \item{estfun}{a matrix containing the empirical estimating functions.}
    \item{n}{number of observations.}
    \item{nobs}{number of observations with non-zero weights.}
    \item{rank}{the numeric rank of the fitted linear model.}
    \item{df.residual}{residual degrees of freedom for fitted model.}
    \item{cov.unscaled}{unscaled covariance matrix for the coefficients.}
    \item{sigma}{residual standard error.}
	\item{y}{the response vector.}
    \item{x}{a list with elements \code{"regressors"}, \code{"instruments"}, \code{"projected"},
      containing the model matrices from the respective components.
	  \code{"projected"} is the matrix of regressors projected
      on the image of the instruments.}
	\item{coefficientsEX}{parameter estimates of the EX analysis}
	\item{residualsEX}{vector of residuals in EX regression} 
    \item{fitted.valuesEX}{vector of fitted values in EX regression}
	\item{vhat}{residuals of first stage of two stage least squared analysis}
	\item{vars}{A list of variable names. List has three vectors for three kinds
		of variables: 'exogenous', 'endogenous' and 'instruments'.}
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Bikram Karmakar
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{\link{ivreg.EX}}, \code{\link{coeftestIV}}, \code{\link{sensIvreg}}, 
	\code{\link[AER]{ivreg.fit}}
}
\examples{
data("CigarettesSW", package = "AER")
CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)


x = CigarettesSW[CigarettesSW[,'year'] == "1995", c('rprice', 'rincome')]
y = CigarettesSW[CigarettesSW[,'year'] == "1995", c('packs')]
z = CigarettesSW[CigarettesSW[,'year'] == "1995", c('tdiff', 'rincome')]

fm3 <- ivreg.fit.EX(as.matrix(x), y, z)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ regression }
\keyword{ IV Model }% __ONLY ONE__ keyword per line
