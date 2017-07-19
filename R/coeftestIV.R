coeftestIV <-
function(ivregfit, lmfit, vars, cls = NULL, vcov){
	
	if(missing(ivregfit) | !("ivregEX" %in% class(ivregfit)))
		stop("ivregfit must be of class 'ivregEX'; an output of ivreg.EX")
	if(missing(lmfit) | !("lm" %in% class(lmfit)))
		stop("lmfit must be of class 'lm'; an output of lm")
	if(missing(vars))
		stop("variable names must be supplied")
	
	if(!is.null(cls)){
		cls = cls[as.numeric(names(ivregfit$fitted.values))]
	}
	
	if(missing(vcov))	
		vcov = sandwich::vcovHC
	
	class(ivregfit) = 'ivreg'
	coefIV = lmtest::coeftest(ivregfit)
	coefIVnonCons = lmtest::coeftest(ivregfit, vcov = vcov)
	
	coefOLS = lmtest::coeftest(lmfit)
	coefOLSnonCons = lmtest::coeftest(lmfit, vcov = vcov)
	
	vars.temp = vars[vars %in% rownames(coefIV) & 
					vars %in% rownames(coefIVnonCons) &
					vars %in% rownames(coefOLS) &
					vars %in% rownames(coefOLSnonCons) ] 
	if(length(vars.temp) == 0)
		stop(paste(vars, "not found in model"))
	vars = vars.temp
	
	coefIV = coefIV[vars,,drop=FALSE]
	coefIVnonCons = coefIVnonCons[vars,,drop=FALSE]
	coefOLS = coefOLS[vars,,drop=FALSE]
	coefOLSnonCons = coefOLSnonCons[vars,,drop=FALSE]

	if(!is.null(cls)) {
		if(is.null(ivregfit$x))
			xz = model.matrix(ivregfit)
		if(!is.null(ivregfit$x))
			xz = ivregfit$x$projected
		Axz = Reduce(f = function(int, i){ 
							int + crossprod(xz[cls==i,]) 
						}, x = unique(cls))
		Bxz = Reduce(f = function(int, i){ 
							v = as.matrix(ivregfit$residuals[cls==i])
							int + t(xz[cls==i,])%*% v %*% t(v) %*% xz[cls==i,]
						}, x = unique(cls))
		IVcoef = ivregfit$coefficients
		sigHeteroIVCls = sqrt(diag(solve(Axz)%*%Bxz%*%solve(Axz)))
		testHeteroIVCls = cbind(IVcoef, sigHeteroIVCls, IVcoef/sigHeteroIVCls, 
						2*pnorm( abs(IVcoef/sigHeteroIVCls), lower.tail=F))
		testHeteroIVCls = testHeteroIVCls[vars,,drop=FALSE]
	}

	coefEx = ivregfit$coefficientsEX[vars]
	sigmaCons = sapply(vars, function(v) sqrt((sum(ivregfit$residualsEX^2)/(ivregfit$n-2))/sum(ivregfit$vhat[,v]^2)))
	sigmaHetero =  sapply(vars, function(v) 
			sqrt((sum(ivregfit$residualsEX^2 * ivregfit$vhat[,v]^2))/sum(ivregfit$vhat[,v]^2)^2))

	testConsvar = cbind(coefEx, sigmaCons, coefEx/sigmaCons, 
							2*pnorm( abs(coefEx/sigmaCons), lower.tail=F))
	testnonConsvar = cbind(coefEx, sigmaHetero, coefEx/sigmaHetero, 
							2*pnorm(abs(coefEx/sigmaHetero), lower.tail=F))

	if(!is.null(cls)){
		sigmaHeteroEXCls = sapply(vars, 
			function(v) sqrt(
				Reduce(f = function(int, i){
				int + (sum(ivregfit$residualsEX[cls==i]^2 * 
					ivregfit$vhat[cls==i,v]^2))/sum(ivregfit$vhat[cls==i,v]^2)^2
					}, x = unique(cls))
				)
			)
		testHeteroEXCls = cbind(coefEx, sigmaHeteroEXCls, coefEx/sigmaHeteroEXCls, 
						2*pnorm(abs(coefEx/sigmaHeteroEXCls), lower.tail=F))
	}

	if(!is.null(cls)){
		x = model.matrix(lmfit)
		Ax = Reduce(f = function(int, i){ 
							int + crossprod(x[cls==i,]) 
						}, x = unique(cls))
		Bx = Reduce(f = function(int, i){ 
				v = as.matrix(lmfit$residuals[cls==i])
				int + t(x[cls==i,])%*% v %*% t(v) %*% x[cls==i,]
				}, x = unique(cls))
		OLScoef = lmfit$coefficients
		sigHeteroOLSCls = sqrt(diag(solve(Ax)%*%Bx%*%solve(Ax)))
		testHeteroOLSCls = cbind(OLScoef, sigHeteroOLSCls, OLScoef/sigHeteroOLSCls, 
						2*pnorm( abs(OLScoef/sigHeteroOLSCls), lower.tail=F))
		testHeteroOLSCls = testHeteroOLSCls[vars,,drop=FALSE]
	}

	if(is.null(cls)){
		summarytests = rbind(coefOLS, coefOLSnonCons, coefIV, coefIVnonCons, 
													testConsvar, testnonConsvar)
		rownames(summarytests) = paste( rep(c('OLS', 'OLS nonCons Var', 'IV', 
				'IV nonCons Var', 'EX', 'EX nonCons Var'), rep(length(vars), 6)),
					 rownames(summarytests), sep='-')
	}
	if(!is.null(cls)){		
		summarytests = rbind(coefOLS, coefOLSnonCons, testHeteroOLSCls, 
					coefIV, coefIVnonCons, testHeteroIVCls,
					testConsvar, testnonConsvar, testHeteroEXCls)
		rownames(summarytests) = paste( rep(c('OLS', 'OLS nonCons Var', 'OLS clustered Var', 
					'IV', 'IV nonCons Var', 'IV clustered Var', 
					'EX', 'EX nonCons Var', 'EX clustered Var'), rep(length(vars), 9)),
					 rownames(summarytests), sep='-')
	}
	
	attr(summarytests, 'method') = "summary of t tests of the coef for ALL the analyses"
	class(summarytests) = "coeftest"
	
	return(summarytests)
}
