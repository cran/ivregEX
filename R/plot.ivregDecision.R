plot.ivregDecision <-
function(x, xaxes = TRUE, yaxes = TRUE, 
					xaxes.label = TRUE, yaxes.label = TRUE, legend = TRUE, ...){
	delta1 = as.numeric(colnames(x))
	delta2 = as.numeric(rownames(x))
	z1 = matrix(NA, nrow(x), ncol(x))
	z1[x == '-'] = 0
	z1[x == '+'] = 1
	z1[x == '*'] = 2
	z1[x == '**'] = 3
	colslist = gray(c(30, 20, 15, 5)/32)
	cols = colslist[1+as.numeric(names(table(z1)))]

	image(delta1, delta2, t(z1), col = cols, xlab = '', ylab = '', axes = F, ...)
	
	xaxs = pretty(delta1)
	yaxs = pretty(delta2)

	if(xaxes) axis(1, at = xaxs , labels = xaxs, ...)
	if(yaxes) axis(2, at = yaxs , labels = yaxs, ...)
	if(xaxes.label) mtext(expression(delta[1]), side=1, line=2.2, ...)
	if(yaxes.label) mtext(expression(delta[2]), side=2, line=2.2, ...)
	range = max(delta1) - min(delta1)
	if(legend)
		mtext(text = c("\u25A0", "no evidence", "\u25A0", "only OLS rejects",
				"\u25A0", "one piece of evidence", "\u25A0", "two pieces of evidence"),
				col = as.vector(rbind(colslist, gray(0))), 
				at = min(delta1) + range*(-.07 + cumsum(c(-.01, .1, .11, .12, .13, .165, .17, .165))), 
				line = 1, cex = c(1.5, .99)[rep(1:2, 4)] )
}
