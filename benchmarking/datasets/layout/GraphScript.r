#### Generate graphs for interpreting encoding comparison study ####
library(ggplot2)
library(pryr)

### Create various boxplots: ####

# returns string w/o leading or trailing whitespace (from Stackoverflow)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

boxplot1topdf <- function(infile, xaxislab=NA, title="",
                          xlab="", ylab="", ylim=c(0,100),
                          fontsizex=2, fontsizel=2, fontsizem=2.5, las=1,
                          mary=6, line=4.5) {
    outfilesplit <- unlist(strsplit(infile, "[/]"))
    filename <- tail(outfilesplit, 1) # assume that last element is filename
    filenamenoending <- substr(filename,1,nchar(filename)-4)
    prepath <- paste(outfilesplit[1:(length(outfilesplit)-1)], sep="/", collapse="/")
    outputpath <- file.path(prepath, "graphs")
    dir.create(outputpath, recursive=TRUE, showWarnings=FALSE)
    fulloutputpath <- paste(outputpath, "/", filenamenoending, ".pdf", sep="")
    indata <- read.csv(infile, header=TRUE, sep=",")
    pdf(fulloutputpath)
    par(mar=c(5,mary,4,2) + 0.1)
    boxplot(indata, data=indata, main=title,
            xlab=xlab, ylim=ylim, las=las,
            cex.main=fontsizem, cex.axis=fontsizex,
            xaxt="n", ann=FALSE)
    if (identical(xaxislab,NA)) xaxislab <- colnames(indata)
    xaxisat=1:length(xaxislab)
    axis(1, at=xaxisat, labels=xaxislab, cex.axis=fontsizex, las=las)
    title(ylab=ylab, cex.lab = fontsizel, line=line)
    # las=1: horizontal (normal) label orientation x axis
    # las=2: vertical label orientation x-axis
    dev.off()
}

barplot1topdf <- function(infile, title="", xlab="", ylab="", ylim=c(0,100), fontsizex=1, las=1) {
    outfilesplit <- unlist(strsplit(infile, "[/]"))
    filename <- tail(outfilesplit, 1) # assume that last element is filename
    filenamenoending <- substr(filename,1,nchar(filename)-4)
    prepath <- paste(outfilesplit[1:(length(outfilesplit)-1)], sep="/", collapse="/")
    outputpath <- file.path(prepath, "graphs")
    dir.create(outputpath, recursive=TRUE, showWarnings=FALSE)
    fulloutputpath <- paste(outputpath, "/", filenamenoending, ".pdf", sep="")
    indata <- read.csv(infile, header=FALSE, sep=",")
    pdf(fulloutputpath)
    bp <- barplot(t(indata[ , -1]), ylim=ylim, las=las, ylab=ylab)
    axis(side = 1, at = bp, labels = indata$V1, las=las)
    # las=1: horizontal (normal) label orientation x axis
    # las=2: vertical label orientation x-axis
    dev.off()
}

### Complete boxplots for getting an overview of the data
for (t in c(10, 30, 60, 120)) {
    listSuccRatePerProver <- list.files(paste("PerProver/", t, "s/", "SuccRate", sep=""), pattern="*.csv", full.names=TRUE)
    listAvgSuccTimePerProver <- list.files(paste("PerProver/", t, "s/", "AvgSuccTime", sep=""), pattern="*.csv", full.names=TRUE)

    listSuccRatePerProverPerCategory <- list.files(paste("PerProverPerCategory/", t, "s/", "SuccRate", sep=""),
                                                   pattern="*.csv", full.names=TRUE)
    listAvgSuccTimePerProverPerCategory <- list.files(paste("PerProverPerCategory/", t, "s/", "AvgSuccTime", sep=""),
                                                      pattern="*.csv", full.names=TRUE)

    mstimeout <- t*1000
    lapply(listSuccRatePerProver, partial(boxplot1topdf, ylab="Success rate in each configuration (%)",
                                          fontsizex=1, fontsizel=1, fontsizem=1, mary=4, line=3))
    lapply(listAvgSuccTimePerProver, partial(boxplot1topdf, ylab="Average time (ms) for successful proofs", ylim=c(0,mstimeout),
                                             fontsizex=1, fontsizel=1, fontsizem=1, mary=4, line=3))

    lapply(listSuccRatePerProverPerCategory, partial(boxplot1topdf, ylab="Success rate in each configuration (%)",
                                                 fontsizex=1, fontsizel=1, fontsizem=1, mary=4, line=3))
    lapply(listAvgSuccTimePerProverPerCategory, partial(boxplot1topdf, ylab="Average time (ms) for successful proofs",
                                                    ylim=c(0,mstimeout), fontsizex=1, fontsizel=1, fontsizem=1, mary=4, line=3))

    listTimesPerProverPerCategory <- list.files(paste("DetailedOverviewPerCat/", t, "s", sep=""),
                           pattern="*.csv", full.names=TRUE)
    lapply(listTimesPerProverPerCategory, partial(boxplot1topdf, ylab="Prover time (ms) per goal", ylim=c(0,mstimeout), las=2,
                                                  fontsizex=1, fontsizel=1, fontsizem=1, mary=4, line=3))


    listIndividualSuccessRatesPerProverPerCategory <- list.files(paste("IndividualConfSuccessRatesPerCat/", t, "s", sep=""),
                        pattern="*.csv", full.names=TRUE)
    lapply(listIndividualSuccessRatesPerProverPerCategory, partial(barplot1topdf, ylab="Success rate (%)", las=2))

    listStratPerformances <- list.files(paste("PerCompStrat/", t, "s", sep=""), pattern="*.csv", full.names=TRUE)
    lapply(listStratPerformances, partial(boxplot1topdf, ylab="Success rate (%)", las=2,
       fontsizex=1, fontsizel=1, fontsizem=1, mary=4, line=3))
    
    
}

### Boxplots for first paper graph (goal categories overview), for all different timeouts
for (t in c("10s", "30s", "60s", "120s")) {
    boxplot1topdf(paste(t, "/", "Graph1/eprover-successrate_per_goalcategory.csv", sep=""),
                  title="Eprover", ylab="Success rate (%)", xaxislab=c("cex", "ex", "ver", "syn", "test"))
    boxplot1topdf(paste(t, "/", "Graph1/princess-successrate_per_goalcategory.csv", sep=""),
                  title="Princess CASC version", ylab="Success rate (%)", xaxislab=c("cex", "ex", "ver", "syn", "test"))
    boxplot1topdf(paste(t, "/", "Graph1/vampire-4.0-successrate_per_goalcategory.csv", sep=""),
                  title="Vampire 4.0", ylab="Success rate (%)", xaxislab=c("cex", "ex", "ver", "syn", "test"))
    boxplot1topdf(paste(t, "/", "Graph1/vampire-3.0-successrate_per_goalcategory.csv", sep=""),
                  title="Vampire 3.0", ylab="Success rate (%)", xaxislab=c("cex", "ex", "ver", "syn", "test"))
}

### Boxplots for second paper graph (sorting alternatives overview all categories), for all different timeouts
for (t in c("10s", "30s", "60s", "120s")) {
    boxplot1topdf(paste(t, "/", "Graph2/eprover-successrate_per_typingconfiguration.csv", sep=""),
                  title="Eprover", ylab="Success rate (%)", xaxislab=c("b", "g", "t"))
    boxplot1topdf(paste(t, "/", "Graph2/princess-successrate_per_typingconfiguration.csv", sep=""),
                  title="Princess CASC version", ylab="Success rate (%)", xaxislab=c("b", "g", "t"))
    boxplot1topdf(paste(t, "/", "Graph2/vampire-3.0-successrate_per_typingconfiguration.csv", sep=""),
                  title="Vampire 3.0", ylab="Success rate (%)", xaxislab=c("b", "g", "t"))
    boxplot1topdf(paste(t, "/", "Graph2/vampire-4.0-successrate_per_typingconfiguration.csv", sep=""),
                  title="Vampire 4.0", ylab="Success rate (%)", xaxislab=c("b", "g", "t"))
}

### Boxplots for third paper graph (variable alternatives overview, counterexample and synthesis), for all different timeouts
for (t in c("10s", "30s", "60s", "120s")) {
    boxplot1topdf(paste(t, "/", "Graph3/eprover-successrate_per_variableconfiguration.csv", sep=""),
                  title="Eprover", ylab="Success rate (%)", xaxislab=c("in", "ne", "np", "u"))
    boxplot1topdf(paste(t, "/", "Graph3/princess-successrate_per_variableconfiguration.csv", sep=""),
                  title="Princess CASC version", ylab="Success rate (%)", xaxislab=c("in", "ne", "np", "u"))
    boxplot1topdf(paste(t, "/", "Graph3/vampire-3.0-successrate_per_variableconfiguration.csv", sep=""),
                  title="Vampire 3.0", ylab="Success rate (%)", xaxislab=c("in", "ne", "np", "u"))
    boxplot1topdf(paste(t, "/", "Graph3/vampire-4.0-successrate_per_variableconfiguration.csv", sep=""),
                  title="Vampire 4.0", ylab="Success rate (%)", xaxislab=c("in", "ne", "np", "u"))
}

### Boxplots for fourth paper graph (simplification alternatives overview), for all different timeouts
for (t in c("10s", "30s", "60s", "120s")) {
    boxplot1topdf(paste(t, "/", "Graph4/eprover-successrate_per_simplificationconfiguration.csv", sep=""),
                  title="Eprover", ylab="Success rate (%)", xaxislab=c("l", "n", "p"))
    boxplot1topdf(paste(t, "/", "Graph4/princess-successrate_per_simplificationconfiguration.csv", sep=""),
                  title="Princess CASC version", ylab="Success rate (%)", xaxislab=c("l", "n", "p"))
    boxplot1topdf(paste(t, "/", "Graph4/vampire-3.0-successrate_per_simplificationconfiguration.csv", sep=""),
                  title="Vampire 3.0", ylab="Success rate (%)", xaxislab=c("l", "n", "p"))
    boxplot1topdf(paste(t, "/", "Graph4/vampire-4.0-successrate_per_simplificationconfiguration.csv", sep=""),
                  title="Vampire 4.0", ylab="Success rate (%)", xaxislab=c("l", "n", "p"))
}

### Boxplots for fifth paper graph (simplification alternatives, timeout-wise, all categories and all provers)
for (t in c("10s", "30s", "60s", "120s")) {
    boxplot1topdf(paste(t, "/", "Graph5/simplificationperformance_allprovers_allcategories.csv", sep=""),
                  title=t, ylab="Success rate (%)", xaxislab=c("l", "n", "p"))
}

### Boxplots for sixth paper graph (performance of all comp strategies, all provers and categories together)
for (t in c("10s", "30s", "60s", "120s")) {
    boxplot1topdf(paste(t, "/", "Graph6/stratperformance_allprovers_allcategories.csv", sep=""), ylab="Success rate (%)",
                  las=2, fontsizex=1, fontsizel=1, mary=4, line=3) 
}








