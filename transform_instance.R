#!/usr/bin/Rscript

symmetry = function (x) {
	return (max(x)-x)
}

normalize = function (x) {
	return (x/max(x))
}

if (! exists ("no_transform_instance")) {
	args = commandArgs(trailingOnly = TRUE)

	usage = "Usage: transform_instance [sn] src dst"

	if (length(args) != 3)
		stop(usage)

	op = args[1]
	src = args[2]
	dst = args[3]

	x = read.table(src)

	if (length(grep ("s", op))>0)
		x = symmetry (x)
	if (length(grep ("n", op))>0)
		x = normalize (x)
	
	write.table (x, dst, row.names=F, col.names = F)
}
