ivreg.EX <-
function (formula, instruments, data, subset, na.action, weights, 
    offset, contrasts = NULL, model = TRUE, ...) 
{
    cl <- match.call()
    if (missing(data)) 
        data <- environment(formula)
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action", "weights", 
        "offset"), names(mf), 0)
    mf <- mf[c(1, m)]
    mf$drop.unused.levels <- TRUE
    if (!missing(instruments)) {
        formula <- as.Formula(formula, instruments)
        cl$instruments <- NULL
        cl$formula <- formula(formula)
    }
    else {
        formula <- as.Formula(formula)
    }
    stopifnot(length(formula)[1] == 1L, length(formula)[2] %in% 
        1:2)
    has_dot <- function(formula) inherits(try(terms(formula), 
        silent = TRUE), "try-error")
    if (has_dot(formula)) {
        f1 <- formula(formula, rhs = 1)
        f2 <- formula(formula, lhs = 0, rhs = 2)
        if (!has_dot(f1) & has_dot(f2)) 
            formula <- as.Formula(f1, update(formula(formula, 
                lhs = 0, rhs = 1), f2))
    }
    mf$formula <- formula
    mf[[1]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    Y <- model.response(mf, "numeric")
    mt <- terms(formula, data = data)
    mtX <- terms(formula, data = data, rhs = 1)
    X <- model.matrix(mtX, mf, contrasts)
    if (length(formula)[2] < 2L) {
        mtZ <- NULL
        Z <- NULL
    }
    else {
        mtZ <- delete.response(terms(formula, data = data, rhs = 2))
        Z <- model.matrix(mtZ, mf, contrasts)
    }
    weights <- model.weights(mf)
    offset <- model.offset(mf)
    if (is.null(offset)) 
        offset <- 0
    if (length(offset) == 1) 
        offset <- rep(offset, NROW(Y))
    offset <- as.vector(offset)
    rval <- ivreg.fit.EX(X, Y, Z, weights, offset, ...)
    rval$call <- cl
    rval$formula <- formula(formula)
    rval$terms <- list(regressors = mtX, instruments = mtZ, full = mt)
    rval$na.action <- attr(mf, "na.action")
    rval$levels <- .getXlevels(mt, mf)
    rval$contrasts <- list(regressors = attr(X, "contrasts"), 
        instruments = attr(Z, "contrasts"))
    if (model) 
        rval$model <- mf
    # if (y) 
        # rval$y <- Y
    # if (x) 
        # rval$x <- list(regressors = X, instruments = Z, projected = rval$x)
    # else rval$x <- NULL
    class(rval) <- c("ivregEX", "ivreg")
    return(rval)
}
