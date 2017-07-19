print.si <-
function(x, effect = 0, ...){
	valinIntr = function(val, Intr){
		(apply(Intr, 1, function(x) ifelse(min(x)< val & max(x) > val, 0, 1) ))
	}
	findSigdigits <- function(x) length(gregexpr("[[:digit:]]", as.character(x))[[1]])

	cat(paste0("Rejection decision for null effect of ", effect, "\n\n"))
	class(x) = "matrix"
	res = data.frame(round(x, findSigdigits(effect) + 2))
	res = cbind(res, valinIntr(effect, x))
	rownames(res) = rownames(x)
	colnames(res) = c("Lower Limit", "Upper Limit", "")
	res[, 3] = factor(res[,3], levels=c(0,1), labels = c('-', '+'))
	print(res)
	cat("\n'-' Sensitive\t '+' Insensitive \n")
}
