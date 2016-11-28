#!/usr/bin/Rscript

#"%wdom%" = function (x, y) all (x <= y)

#wdominates = function (x, points) any (apply (points, 1, "%wdom%", x = x))

#is_stable = function (points) !any (sapply (1:nrow(points), function (i) wdominates (points[i,], points[-i, ])))

# A more efficient version

is_stable = function (points) {
	points = as.data.frame (points)
	message ("Checking for duplicate vectors...")
	if (any (duplicated (points)))
		return (FALSE)
	require (rPref)
	message ("Checking for domination...")
	pareto = Reduce('*', sapply(names(points), function (x) low_(as.name(x))))
	return (length(psel.indices (points, pareto)) == nrow(points))
}

is_gp = function (points)
	return (! any (apply (points, 2, function (x) any (duplicated (x)))))

random_front = function (n, d, type = "concave", id = 1) {
	points = matrix (runif (n*d), ncol = d)

	if (type == "concave") {
		points = points / apply(points, 1, norm, "2")
	}
	else if (type == "convex") {
		points = points / apply(points, 1, norm, "2")
		points = 1-points
	}
	else if (type == "linear") {
		points = points / apply(points, 1, function (x) norm(matrix(x), "O"))
	}
	else {
		error (paste("Unrecognized type", type))
	}

	descr = data.frame (type = type, d = d, n = n, id = id)
	return (list(instance = points, description = descr))
}

random2File = function  (n, d, type, id = 1, mindst, maxdst = NULL) {
	fn = paste (type, "_d", d, "n", n, "_", id, sep="")
	message (paste ("Generating",  fn, "..."))
	x = random_front (n, d, type, id)
	descr = x$description
	minpath = paste (mindst, fn, sep="/")
	write.table(x$instance, minpath, row.names=F, col.names=F)
	if (!is.null(maxdst)) {
		maxpath = paste (maxdst, fn, sep="/")
		write.table (1-x$instance, maxpath, row.names=F, col.names=F)
		descr = rbind (descr, descr)
		descr$dir = c("min", "max")
		descr$path = c(minpath, maxpath)
	}
	else {
		descr$dir = "min"
		descr$path = minpath
	}
	return (descr)
}


if (! exists ("no_generate_random_front")) {
	args = commandArgs(trailingOnly = TRUE)

	usage = "Usage: randomFrontGenerator.R k n d [concave|convex|linear] dst

where:

  k   is the number of instances
  n   is the number of points
  d   is the dimension
  dst is the directory used as destination (./ if missing)"

	if (length(args) < 4)
		stop(usage)

	k = as.numeric(args[1])
	n = as.numeric(args[2])
	d = as.numeric(args[3])
	type = args[4]

	if (length(args) > 4)
		dst = args[5]
	else
		dst = getwd()

	if (iis.na(k) || is.na(n) || is.na(d))
		stop(usage)

	for (id in 1:k)
		random2File (n, d, type, id, dst);
}
