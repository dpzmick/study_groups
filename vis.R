require(lattice)
args <- commandArgs(trailingOnly = TRUE)
infname <- args[1]
outfname <- args[2]

if (length(args) != 2) {
    print("usage Rscript vis.R input_filename output_filename")
    quit()
}

print("Reading from file:")
print(infname)

print("Writing to file:")
print(outfname)
pdf(file=outfname)

data <- read.csv(infname, header=FALSE)
levelplot(data.matrix(data),
          xlab="selflessness", ylab="split chance",
          col.regions=gray(100:0/100))
