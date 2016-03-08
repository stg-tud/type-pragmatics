#### Generate graphs for interpreting encoding comparison study ####
library(ggplot2)
library(pryr)

### Create various boxplots: ####

# returns string w/o leading or trailing whitespace (from Stackoverflow)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

boxplot1topdf <- function(infile, ylab) {
    outfilesplit <- unlist(strsplit(infile, "[/]"))
    filename <- outfilesplit[3]
    title <- substr(filename,1,nchar(filename)-4)
    outputpath <- paste(outfilesplit[1], "/", outfilesplit[2], "/", sep="")
    dir.create(file.path("graphs", outputpath), recursive=TRUE, showWarnings=FALSE)
    fulloutputpath <- paste(outputpath, title, ".pdf", sep="")
    indata <- read.csv(infile, header=TRUE, sep=",")
    pdf(file.path("graphs", fulloutputpath))
    boxplot(indata, data=indata, main=title,
            xlab="Configuration", ylab=ylab)
    dev.off()
}

listSuccRatePerProver <- list.files("PerProver/SuccRate", pattern="*.csv", full.names=TRUE)
listAvgSuccTimePerProver <- list.files("PerProver/AvgSuccTime", pattern="*.csv", full.names=TRUE)

listSuccRatePerProverPerCategory <- list.files("PerProverPerCategory/SuccRate", pattern="*.csv", full.names=TRUE)
listAvgSuccTimePerProverPerCategory <- list.files("PerProverPerCategory/AvgSuccTime", pattern="*.csv", full.names=TRUE)

lapply(listSuccRatePerProver, partial(boxplot1topdf, ylab="Success rate in each configuration (%)"))
lapply(listAvgSuccTimePerProver, partial(boxplot1topdf, ylab="Average time (ms) for successful proofs"))

lapply(listSuccRatePerProverPerCategory, partial(boxplot1topdf, ylab="Success rate in each configuration (%)"))
lapply(listAvgSuccTimePerProverPerCategory, partial(boxplot1topdf, ylab="Average time (ms) for successful proofs"))






