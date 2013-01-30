delayedAssign("ImageJ", {
	res <- try(.jnew("ij/ImageJ"), silent = TRUE)
	if (inherits(res, "try-error")) NULL else res
})
