plot.sensIvregEX <-
function(x, effect = 0, ...){	
	plot.ivregDecisionList(x = x$Z, ...)
	dev.new()
	par.old = par(mfrow = c(3, 1), oma=c(2, 3, 3, 3), mar=c(0, 0, 0,0))
	plot(x$siOLS, effect = effect)
	title("OLS", line = -2) 
	plot(x$si2SLS, effect = effect)
	title("2SLS", line = -2)
	plot(x$siEX, effect = effect)
	title("EX", line = -2)
	par(par.old)
}
