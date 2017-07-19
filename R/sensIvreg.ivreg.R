sensIvreg.ivreg <-
function(ivregfit, coeftestIVres, variable, 
					delta2 = seq(-.1, .1, .01), 
					g = sqrt, errType = c("homoscedastic", "heteroscedastic"),
					alpha = 0.05, ...){
					
	if(missing(coeftestIVres))
		coeftestIVres = lmtest::coeftest(ivregfit)
		
	if(length(variable) != 1) stop('Process only one variable at a time')
	if(!is.numeric(delta2)) stop('delta2 must be a numeric vector')
	
	delta2 = sign(delta2)*sapply(abs(delta2), function(x) min(1, x))
	delta2 = unique(delta2)
	
	coefmat = coeftestIVres
	
	instruments = setdiff(colnames(ivregfit$x$instruments),  colnames(ivregfit$x$projected))
	exogenous = unique(c(setdiff(colnames(ivregfit$x$instruments), instruments),
						setdiff(colnames(ivregfit$x$regressors), variable)))
					
	
	Z = apply(ivregfit$x$instruments[,instruments, drop=FALSE], 1, g)

	lmfitZonhatY = summary(lm(Z ~ ivregfit$x$projected))
	thatY = lmfitZonhatY$coefficients[paste0('ivregfit$x$projected', variable) ,3]
	R2Z.hatYX1 = lmfitZonhatY$r.square
	R2Z.X1 = summary(lm(Z ~ ivregfit$x$projected[,exogenous, drop=FALSE] - 1))$r.square
	rho2 = 1 - R2Z.X1/R2Z.hatYX1
	
	if(length(agrep(variable, rownames(coefmat))) != 1)
		stop(paste(variable, "not found in the model"))
		
	si2SLS = sens2SLS(delta2, df = ivregfit$df.residual, 
						coefmat[agrep(variable, rownames(coefmat)), 1],
						coefmat[agrep(variable, rownames(coefmat)), 2], 
						thatY =thatY , rho2=rho2, alpha = alpha, ...)
						
	findSigdigits <- function(x) length(gregexpr("[[:digit:]]", as.character(x))[[1]])
	significance2 = max(sapply(delta2, findSigdigits))
	rownames(si2SLS) = round(delta2, significance2)
	class(si2SLS) = c("si", "matrix")
	invisible(si2SLS)
	
	
}
