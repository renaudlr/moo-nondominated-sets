#!/usr/bin/Rscript

#A = function (n) cbind (2:(n+1), (n+1):2)

#wfgHard = function (k, p, id = 1) {
	#if (p %% 2 != 0)
		#stop("Argument p must be even.");
	#result = NULL
	#for (i in 1:(p/2)) {
		#column = NULL
		#for (j in 1:(p/2)) {
			#if (i == j) {
				##column = rbind(column, A(k)+(j+i-1)%%(p/2)*(k))
				#column = rbind(column, A(k))
			#}
			#else {
				#column = rbind(column, matrix(1, nrow=k, ncol=2))
			#}
		#}
		#result = cbind(result, column)
	#}
	#result = result/(max(result)+1)
	#descr = data.frame (type = "wfg_hard", d = ncol(result), n = nrow(result), id = id)
	#return (list(instance=result, description = descr))
#}

A = function (n) cbind (1:n, n:1)

wfgHard = function (k, p, id = 1) {
	if (p %% 2 != 0)
		stop("Argument p must be even.");
	result = NULL
	for (i in 1:(p/2)) {
		column = NULL
		for (j in 1:(p/2)) {
			column = rbind(column, A(k)+(j+i-1)%%(p/2)*(k))
		}
		result = cbind(result, column)
	}
	result = result/(max(result)+1)
	descr = data.frame (type = "wfg_harder", d = ncol(result), n = nrow(result), id = id)
	return (list(instance=result, description = descr))
}

wfgHard2file = function (k, p, id = 1, mindst = ".", maxdst = NULL) {
	fn = paste("/wfg_harder_d", p, "n", p/2*k, '_', id, sep="")
	x = wfgHard (k, p)
	descr = x$description
	minpath = paste (mindst, fn, sep="/")
	write.table (x$instance, minpath, row.names = F, col.names = F)
	if (!is.null (maxdst)) {
		maxpath = paste (maxdst, fn, sep="/")
		write.table (1-x$instance, maxpath, row.names = F, col.names = F)
		descr = rbind (descr, descr)
		descr$dir = c("min", "max")
		descr$path = c(minpath, maxpath)
	}
	else {
		descr$dir = "min"
		descr$path = minpath
	}
	message("Generated ", fn)
	return (descr)
}


if (! exists ("no_generate_wfg_hard")) {
	args = commandArgs(trailingOnly = TRUE)

	usage = "Usage: generate_wfg_hard.R k p"

	if (length(args) != 2)
		stop(usage)

	k = as.numeric(args[1])
	p = as.numeric(args[2])

	if (is.na(k) || is.na(p))
		stop(usage)

	wfgHard2file (k, p)
}
