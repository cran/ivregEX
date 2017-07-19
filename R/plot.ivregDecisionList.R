plot.ivregDecisionList <-
function(x, xaxes = TRUE, yaxes = TRUE, 
			xaxes.label = TRUE, yaxes.label = TRUE, legend = TRUE, subtitle =  TRUE, nc, ...){
	
	mfrow_args = get_nrnc(length(x), ifelse(missing(nc), -1, nc))
	par(mfrow = c(mfrow_args[1], mfrow_args[2]))
	nr = par()$mfrow[1]
	nc = par()$mfrow[2]
	
	if(nc > 1 | nr > 1) 
		par.old = par(oma=c(2.8, 2.8, 3, 0), mar=c(1.2, 1.2, .6,.8))
	resPlot = 0	## num Plots in last row is less than nc? NO
	plotLeft = (nc - length(x) %% nc)
	if(plotLeft!=nc){
		resPlot = 1	#YES
		numresPlot = plotLeft/2
		resPotId = nc*(nr-1) + seq(from = 1, length = numresPlot) #Ids of empty plots
	}
	colslist = gray(c(30, 20, 15, 5)/32)
	
	
	
	for(i in 1:length(x)){
		if(resPlot == 1)
			if((i-1) %/% nc == nr - 1 & i %% nc == 1){	
				for(emPlot in seq(from = 1, length = numresPlot))
					plot(c(0,1), xlab = '', ylab = '', axes = F, type = 'n') #null plot
		}
		firstCol = i %% nc == 1
		lastRow = (i-1) %/% nc == nr - 1
		
		z = x[[i]]
		delta1 = as.numeric(colnames(z))
		delta2 = as.numeric(rownames(z))
		z1 = matrix(NA, nrow(z), ncol(z))
		z1[z == '-'] = 0
		z1[z == '+'] = 1
		z1[z == '*'] = 2
		z1[z == '**'] = 3
		
		cols = colslist[1+as.numeric(names(table(z1)))]

		image(delta1, delta2, t(z1), col = cols, xlab = '', ylab = '', axes = F, ...)
					
		xaxs = pretty(delta1)
		yaxs = pretty(delta2)

		if(nc == 1 | firstCol){
			if(yaxes.label) mtext(expression(delta[2]), side=2, line=2.2, ...)
			if(yaxes) axis(2, at = yaxs , labels = yaxs, ...)
		}
		if(!firstCol){
			if(yaxes) axis(2, at = yaxs , labels = FALSE, ...)
		}
		if(lastRow){
			axis(1, at = xaxs , labels = xaxs, ...)
			if(xaxes.label) mtext(expression(delta[1]), side=1, line=2.7, ...)
		}
		if(exists("resPotId"))
		if((i-1) %/% nc == nr - 2 & (i+nc) %in% resPotId){
			axis(1, at = xaxs , labels = xaxs, ...)
			if(xaxes.label) mtext(expression(delta[1]), side=1, line=2.7, ...)
		}
		if(!lastRow){
			axis(1, at = xaxs , labels = FALSE, ...)
		}
		
		if(subtitle) title(paste('Effect = ', names(x[i])), line = -1)
		
	}
	
	if(legend)
		mtext(text = c("\u25A0", "no evidence", "\u25A0", "only OLS rejects",
			"\u25A0", "one piece of evidence", "\u25A0", "two pieces of evidence"),
			col = as.vector(rbind(colslist, gray(0))), 
			at =  cumsum(c(0, .09, .1, .11, .12, .14, .14, .15)),
			cex = c(1.5, .99)[rep(1:2, 4)], line = 1, outer = T )

	if(nc > 1) par(par.old)
	par(mfrow = c(1,1))
}
