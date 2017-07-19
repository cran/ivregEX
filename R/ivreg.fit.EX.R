ivreg.fit.EX <-
function (x, y, z, weights, offset, ...) 
{
    n <- NROW(y)
    p <- ncol(x)
    z <- if (missing(z)) 
        NULL
    else as.matrix(z) 
	#print(head(z))
    if (missing(weights)) 
        weights <- NULL
    if (missing(offset)) 
        offset <- rep(0, n)
    stopifnot(n == nrow(x))
    if (!is.null(z)) 
        stopifnot(n == nrow(z))
    if (!is.null(weights)) 
        stopifnot(n == NROW(weights))
    stopifnot(n == NROW(offset))
    if (!is.null(z)) {
        if (ncol(z) < ncol(x)) 
            warning("more regressors than instruments")
        auxreg <- if (is.null(weights)) 
            lm.fit(z, x, ...)
        else lm.wfit(z, x, weights, ...)
        xz <- as.matrix(auxreg$fitted.values)
        colnames(xz) <- colnames(x)
		
		###############
		xExo = x[,setdiff(colnames(x), setdiff(colnames(x), colnames(z))), drop = FALSE]
		xEndo = x[,setdiff(colnames(x), colnames(z)), drop = FALSE]
		Intru = z[,setdiff(colnames(z), colnames(x)), drop = FALSE]
		#print(setdiff(colnames(z), colnames(x)))
		#print(setdiff(colnames(x), colnames(z)))
		#print(setdiff(colnames(x), setdiff(colnames(x), colnames(z))))
		
	#print(colnames(xExo ))
	#print(colnames(xEndo ))
	#print(colnames(Intru ))
		###############
		#auxregEndo <- if (is.null(weights)) 
        #    lm.fit(xExo, Intru, ...)
        #else lm.wfit(xExo, Intru, weights, ...) 
		#Intrures = Intru - xExo %*% auxregEndo$coefficients
		
		#auxregEX <- if (is.null(weights)) 
        #    lm.fit(Intrures, xEndo, ...)
        #else lm.wfit(Intrures, xEndo, weights, ...)
		#vhat <- xEndo - (Intrures %*% auxregEX$coefficients)
		
	  auxregEX <- if (is.null(weights)) 
            lm.fit(z, xEndo, ...)
        else lm.wfit(z, xEndo, weights, ...)
		vhat <- xEndo - (z %*% auxregEX$coefficients)
       # colnames(vhat) <- colnames(xEndo)	#setdiff(colnames(x), colnames(z))
	 # auxregExonZ <- if (is.null(weights)) 
       #     lm.fit(Intru, xExo, ...)
       # else lm.wfit(Intru, xExo, weights, ...)

		vhat = cbind(vhat, Intru, xExo)	# xExo - Intru %*% auxregExonZ$coefficients)
		colnames(vhat) = c(colnames(xEndo), colnames(Intru), colnames(xExo))
		vhat = vhat[,c(colnames(x), colnames(Intru))]
		#print(summary(vhat))
		#print(Intru)
    }
    else {
        xz <- x
		vhat = NULL
    }
    fit <- if (is.null(weights)) 
        lm.fit(xz, y, offset = offset, ...)
    else lm.wfit(xz, y, weights, offset = offset, ...)
    yhat <- drop(x %*% fit$coefficients)
    names(yhat) <- names(y)
    res <- y - yhat
    ucov <- chol2inv(fit$qr$qr[1:p, 1:p, drop = FALSE])
    colnames(ucov) <- rownames(ucov) <- names(fit$coefficients)
    rss <- if (is.null(weights)) 
        sum(res^2)
    else sum(weights * res^2)
	
	###############
	if(!is.null(vhat)){
		#print(summary(vhat))
		fitEX <- if (is.null(weights)) 
			lm.fit(vhat, y, offset = offset, ...)
		else lm.wfit(vhat, y, weights, offset = offset, ...)
		yhatEX <- drop(vhat %*% fitEX$coefficients)
		names(yhatEX) <- names(y)
		resEX <- y - yhatEX
		#ucovEX <- chol2inv(fitEX$qr$qr[1:p, 1:p, drop = FALSE])
		#colnames(ucovEX) <- rownames(ucovEX) <- names(fitEX$coefficients)
		rssEX <- if (is.null(weights)) 
			sum(resEX^2)
		else sum(weights * resEX^2)
	}
	
    rval <- if(!is.null(vhat)) list(coefficients = fit$coefficients, residuals = res, 
        fitted.values = yhat, weights = weights, offset = if (identical(offset, 
            rep(0, n))) NULL else offset, n = n, nobs = if (is.null(weights)) n else sum(weights > 
            0), rank = fit$rank, df.residual = fit$df.residual, 
        cov.unscaled = ucov, sigma = sqrt(rss/fit$df.residual),		
        x = xz, 
		
		coefficientsEX = fitEX$coefficients, residualsEX = resEX, 
        fitted.valuesEX = yhatEX, vhat = vhat,
		vars = list(exogenous = colnames(xExo), endogenous = colnames(xEndo), instruments = colnames(Intru)))
	else list(coefficients = fit$coefficients, residuals = res, 
        fitted.values = yhat, weights = weights, offset = if (identical(offset, 
            rep(0, n))) NULL else offset, n = n, nobs = if (is.null(weights)) n else sum(weights > 
            0), rank = fit$rank, df.residual = fit$df.residual, 
        cov.unscaled = ucov, sigma = sqrt(rss/fit$df.residual),		
        x = xz)
	
    rval$y <- y
    rval$x <- list(regressors = x, instruments = z, projected = rval$x)
	class(rval) = 'ivregEX'
    return(rval)
}
