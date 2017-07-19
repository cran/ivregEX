sensIvreg.ivregEX <-
function(ivregfit, coeftestIVres, variable,
					delta1 = seq(-.1, .1, .01), delta2 = seq(-.1, .1, .01), 
					effect = 0, 
					g = sqrt, errType = c("homoscedastic", "heteroscedastic"),
					alpha = 0.05, show = FALSE, ...){
	
	if(missing(coeftestIVres) | !("coeftest" %in% class(coeftestIVres)))
		stop("coeftestIVres must be an output of coeftestIV for sensitivity analyses of an ivregEX class object")
		
	if(length(variable) != 1) stop('Process only one variable at a time')
	
	if(!is.numeric(delta1)) stop('delta1 must be a numeric vector')
	if(!is.numeric(delta2)) stop('delta2 must be a numeric vector')
	if(!is.numeric(effect)) stop('effect must be a numeric vector')
	
	findSigdigits <- function(x) length(gregexpr("[[:digit:]]", as.character(x))[[1]])
		
	delta1 = sign(delta1)*sapply(abs(delta1), function(x) min(1, x))
	delta1 = unique(delta1)
	delta2 = sign(delta2)*sapply(abs(delta2), function(x) min(1, x))
	delta2 = unique(delta2)
	
	significance1 = max(sapply(delta1, findSigdigits))
	significance2 = max(sapply(delta2, findSigdigits))

	#if("ivreg.EX" %in% class(ivregfit)){
	
	coefmat = coeftestIVres[agrep(paste0('-',variable), rownames(coeftestIVres)),]
	if(nrow(coefmat) == 0) stop(variable, ' not found in coefficient test')
	if(!(variable %in% colnames(ivregfit$x$projected)))
				stop(variable, 'not found in coefficient test')
	if(length(errType) != 1)
		errType = "homoscedastic"
	if(length(c(agrep("homo*", errType),agrep("hetero*", errType))) != 1)
		stop("errType must be 'homoscedastic' or 'heteroscedastic'")
	if(charmatch("homo", errType, nomatch = -1) == -1)
		coefmat = coefmat[agrep('nonCons Var', rownames(coefmat)),]
	if(charmatch("hetero", errType, nomatch = -1) == -1)
		coefmat = coefmat[-(agrep('nonCons Var', rownames(coefmat))),]
	
	siOLS = sensOLS(delta1, ivregfit$df.residual, 
						coefmat[agrep('OLS*', rownames(coefmat)), 1],
						coefmat[agrep('OLS*', rownames(coefmat)), 2],
						alpha = alpha, ...)
	
	Z = apply(ivregfit$x$instruments[,ivregfit$vars$instruments, drop=FALSE], 1, g)

	lmfitZonhatY = summary(lm(Z ~ ivregfit$x$projected))
	thatY = lmfitZonhatY$coefficients[paste0('ivregfit$x$projected', variable) ,3]
	R2Z.hatYX1 = lmfitZonhatY$r.square
	
	exogenous = unique(c(ivregfit$vars$exogenous, 
							setdiff(ivregfit$vars$endogenous, variable)))
	R2Z.X1 = summary(lm(Z ~ ivregfit$x$projected[, exogenous, drop=FALSE] - 1))$r.square
	rho2 = 1 - R2Z.X1/R2Z.hatYX1		
	
	si2SLS = sens2SLS(delta2, df = ivregfit$df.residual, 
						coefmat[agrep('IV*', rownames(coefmat)), 1],
						coefmat[agrep('IV*', rownames(coefmat)), 2], 
						thatY =thatY , rho2=rho2, alpha = alpha, ...)
	
		
	
	r_1 = summary(lm(ivregfit$x$regressors[,ivregfit$vars$endogenous, 
							drop=FALSE] ~ ivregfit$x$instruments))$r.square
	r_2 = summary(lm(ivregfit$x$regressors[,ivregfit$vars$endogenous, 
							drop=FALSE] ~ ivregfit$x$instruments[,ivregfit$vars$exogenous, 
													drop=FALSE]-1))$r.square
	rhoYX1.X1sq = 1 - r_2/r_1

	degf = ivregfit$df.residual - 
		qr(ivregfit$x$instruments[,ivregfit$vars$instruments, drop=FALSE])$rank
	siEX = sensEX(delta1, degf, 
						coefmat[agrep('EX*', rownames(coefmat)), 1],
						coefmat[agrep('EX*', rownames(coefmat)), 2],  
						rhoYX1.X1sq= rhoYX1.X1sq,
						alpha = alpha, ...)
	
	valinIntr = function(val, Intr){
		(apply(Intr, 1, function(x) ifelse(min(x)< val & max(x) > val, 0, 1) ))
	}


	trtEff = effect
	Z = list()
	Z1 = list()	
	
	for(trtEff in effect){
		z = matrix(valinIntr(trtEff, siOLS), length(delta2), length(delta1), byrow = T)
		z2SLS = matrix(valinIntr(trtEff, si2SLS), length(delta2), length(delta1), byrow = FALSE)
		zEX = matrix(valinIntr(trtEff, siEX), length(delta2), length(delta1), byrow = T)

		z = z + z*(z2SLS) + z*(zEX)
		z1 = data.frame(apply(z, 2, function(z) 
				factor(z, levels = c(0,1,2,3), labels = c('-','+', '*', '**'))))		
		
		colnames(z) = rownames(siOLS) = rownames(siEX) = round(delta1, significance1)
		colnames(z1) = round(delta1, significance1)
		rownames(z) = rownames(si2SLS) = round(delta2, significance2)
		rownames(z1) = round(delta2, significance2) 

		class(z1) = c('ivregDecision', 'data.frame')
		
		Z[[as.character(trtEff)]] = z
		Z1[[as.character(trtEff)]] = z1
	}
	class(Z1) = 'ivregDecisionList'
	#z = as.factor(z)
	if(show) print(Z1)
	class(siOLS) = class(si2SLS) = class(siEX) = "si"
	res = list(Z = Z1, siOLS = siOLS, si2SLS = si2SLS, siEX = siEX)
	class(res) = "sensIvregEX"
	#return(z)
	return(invisible(res))
	#}
		
}
