get_nrnc <-
function(m, nc = -1, choices = 2:5){
	if(nc == -1){
		if(m <= 3){
			nc = m
			nr = 1
		}
		if(m > 3){
			temp = sapply(choices, function(x) m %% x) 
			nc = choices[which(temp == 0)][1]
			if(is.na(nc)){				
				for(xid in 2:length(choices)){
					if(prod(temp[xid] %in% (choices[xid] - 2*1:(choices[xid] %/% 2))))
						nc = choices[xid]
				}
			}
		}
		if(!is.na(nc)){
			nr = m%/%nc + ifelse(m%%nc==0,0,1)
			nc = nc
		}
		if(is.na(nc)){
			nr = 1 + m %/% 4
			nc = 4
		}		
	}
	if(nc != -1){
		nc = min(nc, m)
		nr = m%/%nc + ifelse(m%%nc==0,0,1)
	}
	return(c(nr, nc))
}
