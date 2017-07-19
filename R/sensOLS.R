sensOLS <-
function(delta1, df, est, se, alpha = 0.05, talpha){
	if(missing(talpha)) talpha = qt((1-alpha/2), df)
	siOLS = matrix(NA, length(delta1), 2)
	rownames(siOLS) = delta1
	for(D1 in delta1){
		D = df[1]*D1^2/(1 - D1^2)
		siOLS[which(D1==delta1),] = est + c(-1, 1)*sqrt(D^2 + talpha^2*(1 + (1+D^2)/(df-1)))*se
	}
	siOLS
}
