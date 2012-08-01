response <- function (object, ...)
	UseMethod("response")
	
response.default <- function (object, ...)
	attr(object, "response")
	
train <- function (object, ...)
	UseMethod("train")
	
train.default <- function (object, ...)
	attr(object, "train")

## TODO: test performances of optimized code for Class ~ .
mlearning <- function (formula, data, method, model.args, call = match.call(),
..., subset, na.action = na.fail)
{
	## Our own construction of response vector and terms matrix
	if (missing(model.args))
		model.args <- list(formula  = formula, data = substitute(data),
			subset = substitute(subset))
	
	## Get data and initial number of cases
	data <- eval.parent(model.args$data)
	nobs <- NROW(data)
	
	## Special case for formula like response ~ . which speeds up calc and
	## uses less memory than model.frame()
	isSimpleFormula <- function (formula) {
		vars <- all.vars(formula)
		(length(vars) == 2 && vars[2] == ".") || # Supervised (response ~ .)
		(length(vars) == 1 && vars[1] == ".")	 # Unsupervised (~ .)
	}	
	optim <- isSimpleFormula(model.args$formula)
	if (optim) {
		## data do not need to be changed... except for subset or na.action
		if (model.args$subset != "")
			data <- data[eval.parent(model.args$subset), ] 
		if (missing(na.action) || as.character(na.action) == "") {
			## Use same rules as model.frame():
			## (1) any na.action attribute of data
			na.action <- attr(data, "na.action")
			## (2) option na.action, or (3) na.fail
			if (is.null(na.action))
				na.action <- getOption("na.action", na.fail)
		}
		## Apply provided na.action
		data <- match.fun(na.action)(data)
		if (is.function(na.action)) na.action <- substitute(na.action)
		na.action <- as.character(na.action)
		model.terms <- terms(formula, data = data[1, ])
		attr(data, "terms") <- model.terms
	} else { # Use model.frame()
		if (missing(na.action) || as.character(na.action) == "") {
			data <- do.call("model.frame", model.args)
			na.action <- as.character(attr(data, "na.action"))
			if (!length(na.action)) {
				na.action <- "na.pass" # If not provided, either pass, or no NAs!
			} else na.action <- paste("na", class(na.action), sep = ".")
		} else {
			model.args$na.action <- na.action
			data <- do.call("model.frame", model.args)
			if (is.function(na.action)) na.action <- substitute(na.action)
			na.action <- as.character(na.action)
		}
		model.terms <- attr(data, "terms")
	}
	## Final number of observations
	nobs[2] <- NROW(data)
	names(nobs) <- c("initial", "final")
	
	## Construct the matrix of numeric predictors and the response
	term.labels <- attr(model.terms, "term.labels")
	response.pos <- attr(model.terms, "response")
	if (!response.pos) {
		response.label <- NULL
		train <- data
		response <- NULL
		lev <- NULL
		type <- "unsupervised"
	} else { # Supervised classification or regression
		response.label <- deparse(attr(model.terms, "variables")
			[[response.pos + 1]])
		response <- data[, response.label]
		if (is.factor(response)) {
			lev <- levels(response)
			response <- droplevels(response)
			type <- "classification"
		} else {
			if (!is.numeric(response))
				stop("response variable must be factor or numeric")
			lev <- NULL
			type <- "regression"
		}
		train <- data[, term.labels]
	}
	
	## Construct the mlearning object
	args <- list(...)
	args$type <- type
	args$levels <- lev
	
	## Call the corresponding workhorse function
	res <- match.fun(paste(".", method, sep = ""))(train = train,
		response = response, formula = formula, data = data, args, ...)
		
	## Return a mlearning object
	structure(res$object, formula = formula, train = train, response = response,
		levels = lev, n = nobs, optim = optim, numeric.only = res$numeric.only,
		type = type, pred.type = res$pred.type, summary = res$summary,
		na.action = substitute(na.action), mlearning.call = call,
		method = method, algorithm = res$algorithm, class = res$class)
}

print.mlearning <- function (x, ...)
{
	cat("A mlearning object of class ", class(x)[1], " (",
		attr(x, "algorithm"), "):\n", sep = "")
	type <- attr(x, "type")
	switch(type,
		regression = cat("[regression variant]\n"),
		unsupervised = cat("[unsupervised classification variant]\n"))
	cat("Call: ", deparse(attr(x, "mlearning.call")),
		"\n", sep = "")
	
	if (type == "classification") {
		## Number of cases used
		n <- attr(x, "n")
		if (n["final"] < n["initial"]) {
			msg <- paste("Trained using", n["final"], "out of",
				n["initial"], "cases:")
		} else msg <- paste("Trained using", n["final"], "cases:")
	
		## Categories
		classes <- attr(x, "response")
		levUsed <- levels(classes)
		levIni <- levels(x)
		if (length(levUsed) < length(levIni)) {
			cat("Levels with no cases in the training set that were eliminated:\n")
			print(levIni[!levIni %in% levUsed])
		}

		## Number of cases per used categories
		print(table(classes, dnn = msg))
	}
	return(invisible(x))
}

summary.mlearning <- function (object, ...)
{
	train <- attr(object, "train")
	response <- attr(object, "response")
	mlearning.class <- class(object)[1]
	class(object) <- class(object)[-(1:2)]
	## Summary is sometimes implemented as print() for some machine
	## learning algorithms... this is is 'summary' attribute
	sumfun <- attr(object, "summary")
	if (length(sumfun)) {
		res <- match.fun(sumfun)(object, ...)
	} else res <- object
	class(res) <- c("summary.mlearning", class(res))
	attr(res, "mlearning.class") <- mlearning.class
	attr(res, "algorithm") <- attr(object, "algorithm")
	attr(res, "type") <- attr(object, "type")
	attr(res, "mlearning.call") <- attr(object, "mlearning.call")
	res
}
		
print.summary.mlearning <- function (x, ...)
{
	cat("A mlearning object of class ", attr(x, "mlearning.class"), " (",
		attr(x, "algorithm"), "):\n", sep = "")
	type <- attr(x, "type")
	switch(type,
		regression = cat("[regression variant]\n"),
		unsupervised = cat("[unsupervised classification variant]\n"))
	cat("Initial call: ", deparse(attr(x, "mlearning.call")),
		"\n", sep = "")
	X <- x
	class(X) <- class(x)[-1]
	print(X)
	invisible(x)
}

plot.mlearning <- function (x, y, ...)
{
	train <- attr(x, "train")
	response <- attr(x, "response")
	class(x) <- class(x)[-(1:2)]
	plot(x, ...)
}

.membership <- function (x, levels, scale = TRUE)
{
	## Make sure x is a matrix of numerics
	x <- as.matrix(x)
	if (!is.numeric(x))
		stop("'x' must be numeric")
	
	## Make sure all columns are named with names in levels
	nms <- colnames(x)
	if (!length(nms))
		stop("missing column names in 'x'")
	if (any(!nms %in% levels))
		stop("One or more column in 'x' not in 'levels'")
	
	## Add columns of zeros for inexistant levels
	toAdd <- levels[!levels %in% nms]
	if (length(toAdd)) {
		xAdd <- matrix(0, nrow = NROW(x), ncol = length(toAdd))
		colnames(xAdd) <- toAdd
		x <- cbind(x, xAdd)
	}
	
	## Make sure columns are in the same order as levels
	x <- x[, levels]
	
	## Possibly scale to one, row-wise
	if (isTRUE(as.logical(scale)))
		x <- x / apply(x, 1, sum)
	
	x
}

.expandFactor <- function (f, n, ndrop)
{
	if (!length(ndrop) || class(ndrop) != "exclude") return(f)
	res <- factor(rep(NA, n), levels = levels(f))
	res[-ndrop] <- f
	res
}
	
.expandMatrix <- function (m, n, ndrop)
{
	if (!length(ndrop) || class(ndrop) != "exclude") return(m)
	res <- matrix(NA, nrow = n, ncol = ncol(m))
	res[-ndrop, ] <- m
	res
}

predict.mlearning <- function(object, newdata,
type = c("class", "member", "both"), scale = TRUE, na.action = na.exclude, ...)
{
	## Not usable for unsupervised type
	if (attr(object, "type") == "unsupervised")
		stop("no predict() method for unsupervised version")
	
	## Recalculate newdata according to formula...
	if (missing(newdata)) { # Use train
		newdata <- attr(object, "train")
	} else if (attr(object, "optim")) { # Use optimized approach
		## Just keep vars similar as in train
		vars <- names(attr(object, "train"))
		if (!all(vars %in% names(newdata)))
			stop("one or more missing variables in newdata")
		newdata <- newdata[, vars]
	} else { # Use model.frame
		newdata <- model.frame(formula = attr(object, "formula"),
			data = newdata, na.action = na.pass)[, names(attr(object, "train"))]
	}
	## Do we need only numerical predictors
	if (attr(object, "numeric.only"))
		if (any(sapply(newdata, is.factor))) {
			warning("force conversion from factor to numeric; may be not optimal or suitable")
			newdata <- sapply(as.data.frame(newdata), as.numeric)
		}
	
	## Determine how many data and perform na.action
	n <- NROW(newdata)
	newdata <- match.fun(na.action)(newdata)
	ndrop <- attr(newdata, "na.action")
		
	## Delegate to the original predict() method
	class(object) <- class(object)[-(1:2)]
	if (attr(object, "type") == "regression")
		return(predict(object, newdata = newdata, ...))
	
	## Otherwise, this is a supervised classification
	type <- as.character(type)[1]
	## Special case for both
	if (type == "both") type <- c("class", "member")
	## Check that type is supported and look for corresponding type name
	## in original predict() method
	pred.type <- attr(object, "pred.type")
	if (!all(type %in% names(pred.type)))
		stop("unsupported predict type")
	
	if (length(type) == 2) {
		## Special case where we predict both class and member
		classes <- predict(object, newdata = newdata,
			type = pred.type["class"], ...)
		members <- predict(object, newdata = newdata,
			type = pred.type["member"], ...)
		## Create a list with both res
		levels <- levels(object)
		return(list(class = .expandFactor(factor(as.character(classes),
			levels = levels), n, ndrop),
			member = .expandMatrix(.membership(members, levels = levels,
			scale = scale), n, ndrop)))
	} else {
		res <- predict(object, newdata = newdata, type = pred.type[type], ...)
	}
	
	## Rework result according to initial levels (before drop of empty ones)
	res <- switch(type,
		class = .expandFactor(factor(as.character(res), levels = levels(object)),
			n, ndrop),
		member = .expandMatrix(.membership(res, levels = levels(object),
			scale = scale), n, ndrop),
		switch(class(res)[1],
			factor = .expandFactor(res, n, ndrop),
			matrix = .expandMatrix(res, n, ndrop),
			res))
		
	res
}

## Note: ldahist() in MASS (when only one LD) seems to be broken!
mlLda <- function (formula, data, ..., subset, na.action)
	mlearning(formula, data = data, method = "mlLda", model.args =
		list(formula  = formula, data = substitute(data),
		subset = substitute(subset)), call = match.call(), ...,
		subset = subset, na.action = substitute(na.action))

.mlLda <- function (train, response, formula, data, args, ...)
{
	if (args$type != "classification")
		stop("only factor response (classification) accepted for mlLda")
	list(object = MASS:::lda.default(x = sapply(train, as.numeric),
		grouping = response, ...),
		pred.type = c(class = "class", member = "posterior",
			projection = "x"),
		numeric.only = TRUE, summary = NULL,
		algorithm = "linear discriminant analysis",
		class = c("mlLda", "mlearning", "lda"))
}

predict.mlLda <- function(object, newdata,
type = c("class", "member", "both", "projection"), scale = TRUE,
prior = object$prior, dimension,
method = c("plug-in", "predictive", "debiased"), ...)
{
	if (!inherits(object, "mlLda"))
		stop("'object' must be a 'mlLda' object")
	
	## Recalculate newdata according to formula...
	if (missing(newdata)) { # Use train
		newdata <- attr(object, "train")
	} else if (attr(object, "optim")) { # Use optimized approach
		## Just keep vars similar as in train
		vars <- names(attr(object, "train"))
		if (!all(vars %in% names(newdata)))
			stop("One or more missing variables in newdata")
		newdata <- newdata[, vars]
	} else { # Use model.frame
		newdata <- model.frame(formula = attr(object, "formula"),
			data = newdata, na.action = na.pass)[, names(attr(object, "train"))]
	}
	## Only numerical predictors
	if (any(sapply(newdata, is.factor))) {
		warning("force conversion from factor to numeric; not optimal or suitable predictors")
		newdata <- sapply(as.data.frame(newdata), as.numeric)
	}
	
	## dimension and method
	if (missing(dimension)) {
        dimension <- length(object$svd)
	} else {
		dimension <- min(dimension, length(object$svd))
	}
	method <- as.character(method)[1]
	
	## Delegate to the MASS predict.lda method
	class(object) <- class(object)[-(1:2)]
	## I need to suppress warnings, because NAs produce ennoying warnings!
	res <- suppressWarnings(predict(object, newdata = newdata, prior = prior,
		dimen = dimension, method = method, ...))
	
	## Rework results according to what we want
	switch(as.character(type)[1],
		class = factor(as.character(res$class), levels = levels(object)),
		member = .membership(res$posterior, levels = levels(object),
			scale = scale),
		both = list(class = factor(as.character(res$class),
			levels = levels(object)), member = .membership(res$posterior,
			levels = levels(object), scale = scale)),
		projection = res$x,
		stop("unrecognized 'type' (must be 'class', 'member', 'both' or 'projection')"))
}

mlQda <- function (formula, data, ..., subset, na.action)
	mlearning(formula, data = data, method = "mlQda", model.args =
		list(formula  = formula, data = substitute(data),
		subset = substitute(subset)), call = match.call(), ...,
		subset = subset, na.action = substitute(na.action))

.mlQda <- function (train, response, formula, data, args, ...)
{
	if (args$type != "classification")
		stop("only factor response (classification) accepted for mlQda")
	list(object = MASS:::qda.default(x = sapply(train, as.numeric),
		grouping = response, ...),
		pred.type = c(class = "class", member = "posterior"),
		numeric.only = TRUE, summary = NULL,
		algorithm = "quadratic discriminant analysis",
		class = c("mlQda", "mlearning", "qda"))
}

predict.mlQda <- function(object, newdata, type = c("class", "member", "both"),
prior = object$prior, method = c("plug-in", "predictive", "debiased", "looCV"),
...)
{
	if (!inherits(object, "mlQda"))
		stop("'object' must be a 'mlQda' object")
	
	## Recalculate newdata according to formula...
	if (missing(newdata)) { # Use train
		newdata <- attr(object, "train")
	} else if (attr(object, "optim")) { # Use optimized approach
		## Just keep vars similar as in train
		vars <- names(attr(object, "train"))
		if (!all(vars %in% names(newdata)))
			stop("One or more missing variables in newdata")
		newdata <- newdata[, vars]
	} else { # Use model.frame
		newdata <- model.frame(formula = attr(object, "formula"),
			data = newdata, na.action = na.pass)[, names(attr(object, "train"))]
	}
	## Only numerical predictors
	if (any(sapply(newdata, is.factor))) {
		warning("force conversion from factor to numeric; not optimal or suitable predictors")
		newdata <- sapply(as.data.frame(newdata), as.numeric)
	}
	
	## method
	method <- as.character(method)[1]
	
	## Delegate to the MASS predict.qda method
	class(object) <- class(object)[-(1:2)]
	## I need to suppress warnings, because NAs produce ennoying warnings!
	res <- suppressWarnings(predict(object, newdata = newdata, prior = prior,
		method = method, ...))
	
	## Rework results according to what we want
	switch(as.character(type)[1],
		class = factor(as.character(res$class), levels = levels(object)),
		member = .membership(res$posterior, levels = levels(object),
			scale = scale),
		both = list(class = factor(as.character(res$class),
			levels = levels(object)), member = .membership(res$posterior,
			levels = levels(object), scale = scale)),
		stop("unrecognized 'type' (must be 'class', 'member' or 'both')"))
}

mlRforest <- function (formula, data, ntree = 500, mtry,
replace = TRUE, classwt = NULL, ..., subset, na.action)
{
	if (missing(mtry)) {
		mlearning(formula, data = data, method = "mlRforest", model.args =
			list(formula  = formula, data = substitute(data),
			subset = substitute(subset)), call = match.call(), ntree = ntree,
			replace = replace, classwt = classwt, ...,
			subset = subset, na.action = substitute(na.action))	
	} else {
		mlearning(formula, data = data, method = "mlRforest", model.args =
			list(formula  = formula, data = substitute(data),
			subset = substitute(subset)), call = match.call(), ntree = ntree,
			mtry = mtry, replace = replace, classwt = classwt, ...,
			subset = subset, na.action = substitute(na.action))	
	}
}

.mlRforest <- function (train, response, formula, data, args, ...)
{
	list(object = randomForest:::randomForest.default(x = train,
		y = response, ...),
		pred.type = c(class = "response", member = "prob",
			vote = "vote"),
		numeric.only = FALSE, summary = NULL,
		algorithm = "random forest",
		class = c("mlRforest", "mlearning", "randomForest"))
}

predict.mlRforest <- function(object, newdata,
type = c("class", "member", "both", "vote"),
scale = TRUE, norm.votes = FALSE, oob = FALSE, ...) {
	type <- as.character(type)[1]
	
	if (isTRUE(as.logical(oob))) { # Get out-of-bag prediction!
		if (!missing(newdata))
			stop("you cannot provide newdata when oob = TRUE")
		
		toProps <- function (x, ntree) {
			if (sum(x[1, ] > 1)) {
				res <- t(apply(x, 1, "/", ntree))
			} else res <- x
			class(res) <- "matrix"
			res
		}

		toVotes <- function (x, ntree) {
			if (sum(x[1, ] < ntree - 1)) {
				res <- round(t(apply(x, 1, "*", ntree)))
			} else res <- x
			class(res) <- "matrix"
			res
		}
		
		switch(type,
			class = factor(as.character(object$predicted),
				levels = levels(object)),
			member = .membership(toProps(object$votes, object$ntree),
				levels = levels(object), scale = scale),
			both = list(class = factor(as.character(object$predicted),
				levels = levels(object)),
				member = .membership(toProps(object$votes, object$ntree),
				levels = levels(object), scale = scale)),
			vote = {
				if (isTRUE(as.logical(norm.votes))) {
					.membership(toProps(object$votes, object$ntree),
						levels = levels(object), scale = scale)
				} else {
					.membership(toVotes(object$votes, object$ntree),
						levels = levels(object), scale = scale)
				}
			},
			stop("unknown type, must be 'class', 'member', 'both' or 'vote'"))
		
	} else predict.mlearning(object = object, newdata = newdata,
		type = type, scale = scale, norm.votes = norm.votes, ...)
}

mlNnet <- function (formula, data, size = NULL, rang = NULL, decay = 0,
maxit = 1000, ..., subset, na.action)
	mlearning(formula, data = data, method = "mlNnet", model.args =
		list(formula  = formula, data = substitute(data),
		subset = substitute(subset)), call = match.call(), size = size,
		rang = rang, decay = decay, maxit = maxit, ...,
		subset = subset, na.action = substitute(na.action))

.mlNnet <- function (train, response, formula, data, args, ...)
{

	if (args$type == "unsupervised")
		stop("unsupervised classification not usable for mlNnet")
	argsNames <- names(args)
	args$x <- sapply(train, as.numeric)
	## Weights
	w <- model.weights(data)
	if (length(w) == 0L) w <- rep(1, nrow(train))
	args$weights <- w
			
	## Possibly recalculate best defaults for size and rang
	if (!"size" %in% argsNames || is.null(args$size))
		args$size <- length(levels(response)) - 1 # Is this a reasonable default?
	if (!"rang" %in% argsNames || is.null(args$rang)) {
		## default is 0.7 in original nnet code,
		## but the doc proposes something else
		rang <- round(1 / max(abs(args$x)), 2)
		if (rang < 0.01) rang <- 0.01
		if (rang > 0.7) rang <- 0.7
		args$rang <- rang
	}
			
	## TODO: should I need to implement this???
	#x <- model.matrix(Terms, m, contrasts)
	#cons <- attr(x, "contrast")
	#xint <- match("(Intercept)", colnames(x), nomatch = 0L)
	#if (xint > 0L) 
	#    x <- x[, -xint, drop = FALSE]
			
	## Classification or regression?
	if (is.factor(response)) {
		if (length(levels(response)) == 2L) {
			args$y <- as.vector(unclass(response)) - 1
			args$entropy <- TRUE
			res <- do.call(nnet.default, args)
			res$lev <- args$levels
		} else {
			args$y <- class.ind(response)
			args$softmax <- TRUE
			res <- do.call(nnet.default, args)
		res$lev <- args$levels
		}
	} else { # Regression
		args$y <- response
		res <- do.call(nnet.default, args)	
	}
			
	list(object = res, pred.type = c(class = "class", member = "raw"),
		numeric.only = TRUE, summary = "summary",
		algorithm = "single-hidden-layer neural network",
		class = c("mlNnet", "mlearning", "nnet"))
}

mlLvq <- function (formula, data, k = 5, size, prior, algorithm = "olvq1", ...,
subset, na.action)
{
	if (missing(size)) {
		if (missing(prior)) {
			mlearning(formula, data = data, method = "mlLvq", model.args =
				list(formula  = formula, data = substitute(data),
				subset = substitute(subset)), call = match.call(), k = k,
				algorithm = algorithm, ...,
				subset = subset, na.action = substitute(na.action))
		} else {
			mlearning(formula, data = data, method = "lvq", model.args =
				list(formula  = formula, data = substitute(data),
				subset = substitute(subset)), call = match.call(), k = k,
				prior = prior, algorithm = algorithm, ...,
				subset = subset, na.action = substitute(na.action))
		}
	} else {
		if (missing(prior)) {
			mlearning(formula, data = data, method = "lvq", model.args =
				list(formula  = formula, data = substitute(data),
				subset = substitute(subset)), call = match.call(), k = k,
				size = size, algorithm = algorithm, ...,
				subset = subset, na.action = substitute(na.action))
		} else {
			mlearning(formula, data = data, method = "lvq", model.args =
				list(formula  = formula, data = substitute(data),
				subset = substitute(subset)), call = match.call(), k = k,
				size = size, prior = prior, algorithm = algorithm, ...,
				subset = subset, na.action = substitute(na.action))
		}
	}
}

.mlLvq <- function (train, response, formula, data, args, ...)
{
	argsNames <- names(args)
	if (args$type != "classification")
		stop("only factor response (classification) accepted for mlLvq")
			
	## matrix of numeric values
	train <- sapply(train, as.numeric)
			
	## Default values for size and prior, if not provided
	n <- nrow(train)
	if ("k" %in% argsNames) {
		k <- args$k
	} else k <- 5 # Default value
	if ("prior" %in% argsNames) {
		prior <- args$prior
	} else prior <- tapply(rep(1, length(response)), response, sum)/n
	if ("size" %in% argsNames) {
		size <- args$size
	} else {
		np <- length(prior)  
		size <- min(round(0.4 * np * (np - 1 + ncol(train) / 2), 0), n)
	}
			
	## Initialize codebook
	init <- lvqinit(train, response, k = k, size = size, prior = prior)
	
	## Calculate final codebook
	if ("algorithm" %in% argsNames) {
		algorithm <- as.character(args$algorithm)[1]
	} else algorithm <- "olvq1" # Default algorithm
	if (algorithm == "olvq1") times <- 40 else times <- 100
	if ("niter" %in% argsNames) {
		niter <- args$niter
	} else niter <- times * nrow(init$x) # Default value
	if ("alpha" %in% argsNames) {
		alpha <- args$alpha
	} else alpha <- if (algorithm == "olvq1") 0.3 else 0.03
	if ("win" %in% argsNames) {
		win <- args$win
	} else win <- 0.3
	if ("epsilon" %in% argsNames) {
		epsilon <- args$epsilon
	} else epsilon <- 0.1
	codebk <- switch(algorithm,
		olvq1 = olvq1(train, response, init, niter = niter,
			alpha = alpha),
		lvq1 = lvq1(train, response, init, niter = niter,
			alpha = alpha),
		lvq2 = lvq2(train, response, init, niter = niter,
			alpha = alpha, win = win),
		lvq3 = lvq3(train, response, init, niter = niter,
			alpha = alpha, win = win, epsilon = epsilon),
		stop("algorithm must be 'lvq1', 'lvq2', 'lvq3' or 'olvq1'"))
			
	list(object = codebk, pred.type = c(class = "class"),
		numeric.only = TRUE, summary = "summary.lvq",
		algorithm = "learning vector quantization",
		class = c("mlLvq", "mlearning", class(codebk)))
}

summary.lvq <- function (object, ...)
	structure(cbind(Class = object$cl, as.data.frame(object$x)),
		class = c("summary.lvq", "data.frame"))

print.summary.lvq <- function (x, ...)
{
	cat("Codebook:\n")
	print(as.data.frame(x))
	invisible(x)
}

predict.mlLvq <- function (object, newdata, type = "class",
na.action = na.exclude, ...)
{
	if (!inherits(object, "mlLvq"))
		stop("'object' must be a 'mlLvq' object")
	if (type != "class") stop("Only 'class' currently supported for type")
    
		## Recalculate newdata according to formula...
	if (missing(newdata)) { # Use train
		newdata <- attr(object, "train")
	} else if (attr(object, "optim")) { # Use optimized approach
		## Just keep vars similar as in train
		vars <- names(attr(object, "train"))
		if (!all(vars %in% names(newdata)))
			stop("one or more missing variables in newdata")
		newdata <- newdata[, vars]
	} else { # Use model.frame
		newdata <- model.frame(formula = attr(object, "formula"),
			data = newdata, na.action = na.pass)[, names(attr(object, "train"))]
	}
	newdata <- sapply(as.data.frame(newdata), as.numeric)
	
	## Determine how many data and perform na.action
	n <- NROW(newdata)
	newdata <- match.fun(na.action)(newdata)
	ndrop <- attr(newdata, "na.action")
	
	.expandFactor(lvqtest(object, newdata), n, ndrop)
}

## NaiveBayes from e1071 package
mlNaiveBayes <- function(formula, data, laplace = 0, ..., subset, na.action)
	mlearning(formula, data = data, method = "mlNaiveBayes", model.args =
		list(formula  = formula, data = substitute(data),
		subset = substitute(subset)), call = match.call(), laplace = laplace,
		..., subset = subset, na.action = substitute(na.action))

.mlNaiveBayes <- function (train, response, formula, data, args, ...)
{
	if (args$type != "classification")
		stop("only factor response (classification) accepted for mlNaiveBayes")
			
	list(object = naiveBayes(x = train, y = response, ...),
		pred.type = c(class = "class", member = "raw"),
		numeric.only = FALSE, summary = NULL,
		algorithm = "naive Bayes classifier",
		class = c("mlNaiveBayes", "mlearning", "naiveBayes"))
}
	
## NaiveBayes from RWeka package
mlNaiveBayesWeka <- function(formula, data, ..., subset, na.action)
	mlearning(formula, data = data, method = "mlNaiveBayesWeka", model.args =
		list(formula  = formula, data = substitute(data),
		subset = substitute(subset)), call = match.call(), ...,
		subset = subset, na.action = substitute(na.action))

.mlNaiveBayesWeka <- function (train, response, formula, data, args, ...)
{
	if (args$type != "classification")
		stop("only factor response (classification) accepted for mlNaiveBayesWeka")
			
	wekaArgs <- list(control = args$control)
	
	## If response is not NULL, add it to train
	if (length(response)) {
		response.label <- all.vars(formula)[1]
		data <- data.frame(response, train)
		names(data) <- c(response.label, colnames(train))
		wekaArgs$data <- data
		wekaArgs$formula <- as.formula(paste(response.label, "~ ."))
	} else { # Unsupervised classification
		wekaArgs$data <- train
		wekaArgs$formula <- ~ . 
	}
	WekaClassifier <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
	
	list(object = do.call(WekaClassifier, wekaArgs),
		#x = sapply(model.frame[, term.labels], as.numeric),
		#y = as.factor(model.frame[, response.label]), ...),
		pred.type = c(class = "class", member = "probability"),
		numeric.only = FALSE, summary = "summary",
		algorithm = "Weka naive Bayes classifier",
		class = c("mlNaiveBayesWeka", "mlearning", "Weka_classifier"))
}
