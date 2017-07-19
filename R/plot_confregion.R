plot_confregion <- function(est, se, t, roundto = 3, xlims, ylims = c(0, .7), 
			ypos = .35, xaxis = T, legend = T, 
			xlab = 'Treatment Effect', xlab.cex = 1.3, oma = c(4.5, 0, 2, 0), ...){


	if(missing(est) | missing(se)) stop('est and/or se not provided')
	stopifnot(length(est) == 3 & length(se) == 3)

	if(missing(t)) t = 2

	L = U = rep(NA, 3)
	## set up the intervals
	L[3] = min(est - t*se)
	L[1] = est[1] - t*se[1]
	L[2] = min(max((est - t*se)[2:3]), L[1])

	U[1] = est[1] + t*se[1]
	U[2] = max(U[1], min((est - t*se)[2:3]))
	U[3] = max(est + t*se)

	if(missing(xlims))
		xlims = c(min(L) - (U[1]-L[1]), max(U) + (U[1]-L[1]))
	if(missing(ylims))
		ylims = c(0, max(.7, ypos))

	if(legend){
		dev.new(width=1052, height = 700)
		layout(matrix(c(1,2), 2,1), heights = c(1, 8))
		par(mar = c(.2, .2, par()$mar[c(2,4)])[c(1, 3, 2, 4)], oma = oma)
		
		plot(c(0.5,0.5), type = 'n', axes = F, xlab = NA, ylab = NA, xlim = c(0,1), ylim = c(0,1))

		rect(xleft = 0, ybottom = .6, xright = .05, ytop = 1)
		text(x = .06, y = .85, labels = 'No evidence', pos = 4, cex=1)

		rect(xleft = .23, ybottom = .6, xright = .28, ytop = 1)
		segments(x0 = .24, x1 = .27, y0 = .8, lty = 2)
		text(x = .29, y = .85, labels = 'OLS shows', pos = 4, cex=1)
		text(x = .29, y = .5, labels = 'significance', pos = 4, cex=1)


		rect(xleft = .46, ybottom = .5, xright = .51, ytop = 1)
		segments(x0 = .47, x1 = .5, y0 = .8, lwd = 3)
		text(x = .52, y = .85, labels = 'One piece of', pos = 4, cex=1)
		text(x = .52, y = .5, labels = 'secondary evidence', pos = 4, cex=1)


		rect(xleft = .76, ybottom = .5, xright = .81, ytop = 1)
		segments(x0 = .77, x1 = .80, y0 = .8, lwd = 5)
		text(x = .82, y = .85, labels = 'Two pieces of', pos = 4, cex=1)
		text(x = .82, y = .5, labels = 'secondary evidence', pos = 4, cex=1)


	}


	if(xaxis)
	plot(c(min(L), 0), yaxt = 'n', xaxs = 'i', xlim = xlims, ylim = ylims, type = 'n',
			xlab = NA, ylab = NA, ...)

	if(!is.na(xlab) & xlab != FALSE){
			mtext(ifelse(xlab == TRUE, 'Treatment effect', xlab), side = 1, line = 2, cex = xlab.cex)
	}
	
	if(!xaxis)
		plot(c(min(L), 0), axes = F, xlim = xlims, ylim = ylims, type = 'n',
			xlab = NA, ylab = NA, ...)

	segments(x0 = L[1], x1 = L[2], y0 = ypos, y1 = ypos , lty=2)
	segments(x0 = L[2], x1 = L[3], y0 = ypos , y1 = ypos , lty=1, lwd = 3)
	segments(x0 = L[3], x1 = xlims[1], y0 = ypos , y1 = ypos , lty=1, lwd = 5)


	segments(x0 = U[1], x1 = U[2], y0 = ypos, y1 = ypos , lty=2)
	segments(x0 = U[2], x1 = U[3], y0 = ypos, y1 = ypos, lty=1, lwd = 3)
	segments(x0 = U[3], x1 = xlims[2], y0 = ypos, y1 = ypos, lty=1, lwd = 5)

	text(x = L[3], y = ypos, labels = round(L[3],roundto ), pos = 3)
	text(x = U[3], y = ypos, labels = round(U[3],roundto ), pos = 3)

	if(round(L[2],roundto) != round(L[3],roundto))
		text(x = L[2], y = ypos, labels = round(L[2],roundto ), pos = 1)
	if(round(U[2],roundto) != round(U[3],roundto))
		text(x = U[2], y = ypos, labels = round(U[2],roundto ), pos = 1)

	if(round(L[1],roundto) !=round(L[2],roundto))
		text(x = L[1], y = ypos, labels = round(L[1],roundto ), pos = 4)
	if(round(U[1],roundto) != round(U[2], roundto))
		text(x = U[1], y = ypos, labels = round(U[1],roundto ), pos = 4)




}

