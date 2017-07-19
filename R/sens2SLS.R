sens2SLS <-
function(delta2, df, est, se, thatY, rho2, alpha = 0.05, talpha){
	if(missing(talpha)) talpha = qt((1-alpha/2), df)
	
	si2SLS = matrix(NA, length(delta2), 2)
	rownames(si2SLS) = delta2
	
	for(r in delta2){
		R = r^2
		if(1/R > 1 + (talpha^2/thatY^2)*(df/(df - 1))/(1 - rho2))
			si2SLS[which(r == delta2),] = est + 
						c(-1, 1)*se*(sign(thatY)*thatY*sqrt(R) 
							+ talpha*sqrt(df/(df-1))*sqrt(1 - R)/sqrt(1 - rho2))
		else
			si2SLS[which(r == delta2),] = est + c(-1, 1)*se*
								sqrt(thatY^2 + talpha^2*(df/(df - 1))/(1 - rho2))
	}
	si2SLS
}
