print.ivregDecision <-
function(x, ...){
	cat('Sensitivity analyses for the OLS, 2SLS and EX analyses\n\n')
	class(x) = "data.frame"
	print(x)
	cat('\n')
	cat("'-' no evidence, \t   '+'  only OLS rejects, 
	'*' one piece of evidence, '**' two pieces of evidence.\n")
}
