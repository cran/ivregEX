plot.si <-
function(x, effect = 0, ...){
	valinIntr = function(val, Intr){
		(apply(Intr, 1, function(x) ifelse(min(x)< val & max(x) > val, 0, 1) ))
	}
		
	plot(c(0,1), axes = FALSE, type = 'n', xlab = '', ylab = '')
	par("usr")
	plotPos = (par("usr")[4] + par("usr")[3])/2
	yrange = (par("usr")[4] - par("usr")[3])
	abline(h = plotPos, lwd = 2)
	x0 = par("usr")[1]
	x1 = par("usr")[2]
	y0 = par("usr")[3]
	y1 = par("usr")[4]

	res = valinIntr(effect, x)
	delta = as.numeric(names(res))
	sensitive = delta[res == 0]
	insensitive = delta[res == 1]

	xshift = (x1 - x0)*.15/2
	
	segments(x0 = (x1 + x0)/2, x1 = (x1 + x0)/2, y0 = plotPos - yrange*.025, y1 = plotPos + yrange*.025, lwd = 2)
	text(x =  (x1 + x0)/2, y = plotPos - yrange*.1, label = 0, cex = 1.5)

	if(length(insensitive) >0){
		if((max(abs(delta)) - max(abs(insensitive)))/(2*max(abs(delta))) <= .1)
			delta = insensitive
		if((max(abs(delta)) - max(abs(insensitive)))/(2*max(abs(delta))) > .39){
			delta = c(insensitive, max(abs(insensitive))*2)
		}
		
		if(max(insensitive) <= .9){
			segments(x0 = x0, x1 = x0 + yrange*.025, y0 = plotPos, y1 = plotPos + sqrt(2)*yrange*.025, lwd = 2)
			segments(x0 = x0, x1 = x0 + yrange*.025, y0 = plotPos, y1 = plotPos - sqrt(2)*yrange*.025, lwd = 2)
			segments(x0 = x1, x1 = x1 - yrange*.025, y0 = plotPos, y1 = plotPos + sqrt(2)*yrange*.025, lwd = 2)
			segments(x0 = x1, x1 = x1 - yrange*.025, y0 = plotPos, y1 = plotPos - sqrt(2)*yrange*.025, lwd = 2)			
		}
		if(max(insensitive) > .9){
			delta = insensitive
		}
		if(max(abs(delta)) != max(abs(insensitive))){
			segments(x0 = x0 + xshift, x1 = x0 + xshift, y0 = plotPos - yrange*.025, y1 = plotPos + yrange*.025, lwd = 2)
			text(x = x0 + xshift, y = plotPos - yrange*.1, label = -max(abs(delta)), cex = 1.5)

			segments(x0 =  x1 - xshift, x1 = x1 - xshift, y0 = plotPos - yrange*.025, y1 = plotPos + yrange*.025, lwd = 2)
			text(x = x1 - xshift, y = plotPos - yrange*.1, label = max(abs(delta)), cex = 1.5)
		}

		mindeltaLine = x0 + xshift + (x1 - xshift - (x0 + xshift))*(-max(abs(insensitive)) + max(abs(delta)))/(2*max(abs(delta)))
		maxdeltaline = x0 + xshift + (x1 - xshift - (x0 + xshift))*(max(abs(insensitive)) + max(abs(delta)))/(2*max(abs(delta)))

		segments(x0 = mindeltaLine , x1 = mindeltaLine , y0 = plotPos - yrange*.05, y1 = plotPos + yrange*.05, lwd = 2)
		segments(x0 = mindeltaLine , x1 = mindeltaLine + xshift*.1, y0 = plotPos - yrange*.05, y1 = plotPos - yrange*.05, lwd = 2)
		segments(x0 = mindeltaLine , x1 = mindeltaLine + xshift*.1, y0 = plotPos + yrange*.05, y1 = plotPos + yrange*.05, lwd = 2)
		text(x =  mindeltaLine , y = plotPos - yrange*.1, label = -max(abs(insensitive)), cex = 1.5)

		segments(x0 = maxdeltaline , x1 = maxdeltaline , y0 = plotPos - yrange*.05, y1 = plotPos + yrange*.05, lwd = 2)
		segments(x0 = maxdeltaline , x1 = maxdeltaline - xshift*.1, y0 = plotPos - yrange*.05, y1 = plotPos - yrange*.05, lwd = 2)
		segments(x0 = maxdeltaline , x1 = maxdeltaline - xshift*.1, y0 = plotPos + yrange*.05, y1 = plotPos + yrange*.05, lwd = 2)

		text(x =  maxdeltaline , y = plotPos - yrange*.1, label = max(abs(insensitive)), cex = 1.5)

		
		text(x =  (x1 + x0)/2 , y = plotPos + yrange*.13, label = "Insensitive", cex = 1.5)
		text(x =  (x1 + x0)/2 , y = plotPos - yrange*.2, label = paste0("Null Effect = ", effect), cex = 1.5)
	}
	
	if(length(insensitive) == 0){
		segments(x0 = x0 + xshift, x1 = x0 + xshift, y0 = plotPos - yrange*.025, y1 = plotPos + yrange*.025, lwd = 2)
		text(x = x0 + xshift, y = plotPos - yrange*.1, label = -max(abs(delta)), cex = 1.5)

		segments(x0 =  x1 - xshift, x1 = x1 - xshift, y0 = plotPos - yrange*.025, y1 = plotPos + yrange*.025, lwd = 2)
		text(x = x1 - xshift, y = plotPos - yrange*.1, label = max(abs(delta)), cex = 1.5)

		text(x =  (x1 + x0)/2 , y = plotPos + yrange*.13, label = paste0("Reject Null Effect = ", effect), cex = 1.5)
	}
	
}
