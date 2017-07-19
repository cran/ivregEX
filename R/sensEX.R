sensEX <-
function(delta1, df, est, se, rhoYX1.X1sq, alpha = 0.05, talpha){
	if(missing(talpha)) talpha = qt((1-alpha/2), df)
	siEX = matrix(NA, length(delta1), 2)
	rownames(siEX) = delta1
	for(D1 in delta1){
		D = D1/sqrt(1-rhoYX1.X1sq)
		D = df*D^2/(1 - D^2)
		siEX[which(D1==delta1),] = est + c(-1, 1)*sqrt(D^2 + talpha^2*(1 + (1+D^2)/(df-1)))*se
	}
	siEX
}
