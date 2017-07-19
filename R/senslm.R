senslm <-
function(lmfit, coefmat, variable, delta1 = seq(-.1, .1, .01), alpha = 0.05, ...){
	if(missing(lmfit) | !("lm" %in% class(lmfit)))
		stop("lmfit must be of class 'lm'; an output of lm")
	if(missing(coefmat))
		coefmat = lmtest::coeftest(lmfit)
	if(missing(lmfit) | !("lm" %in% class(lmfit)))
		stop("lmfit must be of class 'lm'; an output of lm")
		
	if(length(variable) != 1) stop('Process only one variable at a time')
	if(!is.numeric(delta1)) stop('delta1 must be a numeric vector')
	
	delta1 = sign(delta1)*sapply(abs(delta1), function(x) min(1, x))
	delta1 = unique(delta1)
	
	if(length(agrep(variable, rownames(coefmat))) != 1)
		stop(paste(variable, "not found in the model"))
		
	siOLS = sensOLS(delta1, lmfit$df.residual, 
						coefmat[agrep(variable, rownames(coefmat)), 1],
						coefmat[agrep(variable, rownames(coefmat)), 2],
						alpha = alpha, ...)
	findSigdigits <- function(x) length(gregexpr("[[:digit:]]", as.character(x))[[1]])
	significance1 = max(sapply(delta1, findSigdigits))
	rownames(siOLS) = round(delta1, significance1)
	class(siOLS) = c("si", "matrix")
	invisible(siOLS)
}
