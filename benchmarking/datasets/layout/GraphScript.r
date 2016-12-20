#### Generate graphs for interpreting encoding comparison study ####
library(ggplot2)
library(pryr)

### Create various boxplots: ####
### Parse command line arguments
# expected 1 argument: folder with layouted .csv files
# for merged SQL/QL results: expects "Merged" as parameter (should also be folder name of merged SQL/QL results)
args <- commandArgs(trailingOnly = TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (folder with layouted .csv files)", call.=FALSE)
}

layoutpath <- args[1]

# returns string w/o leading or trailing whitespace (from Stackoverflow)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

boxplot1topdf <- function(infile, xaxislab=NA, title="",
                          xlab="", ylab="", ylim=c(0,100),
                          fontsizex=2, fontsizel=2, fontsizem=2.5, las=1,
                          mary=6, line=4.5, col="white") {
    outfilesplit <- unlist(strsplit(infile, "[/]"))
    filename <- tail(outfilesplit, 1) # assume that last element is filename
    filenamenoending <- substr(filename,1,nchar(filename)-4)
    prepath <- paste(outfilesplit[1:(length(outfilesplit)-1)], sep="/", collapse="/")
    outputpath <- file.path(prepath, "graphs")
    dir.create(outputpath, recursive=TRUE, showWarnings=FALSE)
    fulloutputpath <- paste(outputpath, "/", filenamenoending, ".pdf", sep="")
    indata <- read.csv(infile, header=TRUE, sep=",")
    if (colnames(indata)[1] == "casestudy") {
        indata <- indata[,-c(1)] #deletes first column if that column just denotes the case study
    }
    pdf(fulloutputpath)
    par(mar=c(5,mary,4,2) + 0.1)
    boxplot(indata, data=indata, main=title,
            xlab=xlab, ylim=ylim, las=las,
            cex.main=fontsizem, cex.axis=fontsizex,
            xaxt="n", ann=FALSE, outline=FALSE, col=col)
    if (identical(xaxislab,NA)) xaxislab <- colnames(indata)
    xaxisat=1:length(xaxislab)
    axis(1, at=xaxisat, labels=xaxislab, cex.axis=fontsizex, las=las)
    title(ylab=ylab, cex.lab = fontsizel, line=line)
    # las=1: horizontal (normal) label orientation x axis
    # las=2: vertical label orientation x-axis
    dev.off()
}

mergedboxplot1topdf <- function(infile, xaxislab=NA, title="",
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
    sqldata <- indata[indata$casestudy=="SQL", -1]
    qldata <- indata[indata$casestudy=="QL", -1]
    par(mar=c(5,mary,4,2) + 0.1) #has to be directly before boxplot!
    boxplot(indata[,-1], ylim=ylim, xlim = c(0.5, ncol(indata[,-1])+0.5),
        boxfill=rgb(1, 1, 1, alpha=1), border=rgb(1, 1, 1, alpha=1),
        yaxt="n", xaxt="n") #invisible boxes
    boxplot(sqldata, ylim=ylim, las=las, xaxt = "n", add = TRUE,
        boxfill="white", boxwex=0.25, at = 1:ncol(indata[,-1]) - 0.15,
        yaxt="n", outline=FALSE) #shift these left by -0.15
    boxplot(qldata, main=title, ylim=ylim, las=las, cex.main=fontsizem,
        cex.axis=fontsizex, xaxt = "n", add = TRUE, boxfill="grey", boxwex=0.25,
        at = 1:ncol(indata[,-1]) + 0.15, outline=FALSE) #shift these right by +0.15
    if (identical(xaxislab,NA)) xaxislab <- colnames(indata)
    xaxisat=1:length(xaxislab)
    axis(1, at=xaxisat, labels=xaxislab, cex.axis=fontsizex, las=las)
    title(ylab=ylab, cex.lab=fontsizel, line=line)
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
    # graphs for comparing encoding strategies only, with axiom selection strategy = selectall
    listSuccRatePerProver <- list.files(paste(layoutpath, "/PerProver/", t, "s/", "SuccRate", sep=""), pattern="*.csv", full.names=TRUE)
    listAvgSuccTimePerProver <- list.files(paste(layoutpath, "/PerProver/", t, "s/", "AvgSuccTime", sep=""), pattern="*.csv", full.names=TRUE)

    listSuccRatePerProverPerCategory <- list.files(paste(layoutpath, "/PerProverPerCategory/", t, "s/", "SuccRate", sep=""),
                                                   pattern="*.csv", full.names=TRUE)
    listAvgSuccTimePerProverPerCategory <- list.files(paste(layoutpath, "/PerProverPerCategory/", t, "s/", "AvgSuccTime", sep=""),
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

    listTimesPerProverPerCategory <- list.files(paste(layoutpath, "/DetailedOverviewPerCat/", t, "s", sep=""),
                           pattern="*.csv", full.names=TRUE)
    lapply(listTimesPerProverPerCategory, partial(boxplot1topdf, ylab="Prover time (ms) per goal", ylim=c(0,mstimeout), las=2,
                                                  fontsizex=1, fontsizel=1, fontsizem=1, mary=4, line=3))


    listIndividualSuccessRatesPerProverPerCategory <- list.files(paste(layoutpath, "/IndividualConfSuccessRatesPerCat/", t, "s", sep=""),
                        pattern="*.csv", full.names=TRUE)
    lapply(listIndividualSuccessRatesPerProverPerCategory, partial(barplot1topdf, ylab="Success rate (%)", las=2))

    listStratPerformances <- list.files(paste(layoutpath, "/PerCompStrat/", t, "s", sep=""), pattern="*.csv", full.names=TRUE)
    lapply(listStratPerformances, partial(boxplot1topdf, ylab="Success rate (%)", las=2,
       fontsizex=1, fontsizel=1, fontsizem=1, mary=4, line=3))

    # graphs for comparing axiom selection strategies
    listSelSuccRatePerProver <- list.files(paste(layoutpath, "/AxiomSelection/PerProver/", t, "s/", "SuccRate", sep=""), pattern="*.csv", full.names=TRUE)
    listSelAvgSuccTimePerProver <- list.files(paste(layoutpath, "/AxiomSelection/PerProver/", t, "s/", "AvgSuccTime", sep=""), pattern="*.csv", full.names=TRUE)

    listSelGoodEncSuccRatePerProver <- list.files(paste(layoutpath, "/AxiomSelection/PerProverGood/", t, "s/", "SuccRate", sep=""), pattern="*.csv", full.names=TRUE)
    listSelGoodEncAvgSuccTimePerProver <- list.files(paste(layoutpath, "/AxiomSelection/PerProverGood/", t, "s/", "AvgSuccTime", sep=""), pattern="*.csv", full.names=TRUE)

    listSelSuccRatePerProverPerCategory <- list.files(paste(layoutpath, "/AxiomSelection/PerProverPerCategory/", t, "s/", "SuccRate", sep=""),
                                                       pattern="*.csv", full.names=TRUE)
    listSelAvgSuccTimePerProverPerCategory <- list.files(paste(layoutpath, "/AxiomSelection/PerProverPerCategory/", t, "s/", "AvgSuccTime", sep=""),
                                                          pattern="*.csv", full.names=TRUE)

    listSelGoodEncSuccRatePerProverPerCategory <- list.files(paste(layoutpath, "/AxiomSelection/PerProverPerCategoryGood/", t, "s/", "SuccRate", sep=""),
                                                                                                                 pattern="*.csv", full.names=TRUE)
    listSelGoodEncAvgSuccTimePerProverPerCategory <- list.files(paste(layoutpath, "/AxiomSelection/PerProverPerCategoryGood/", t, "s/", "AvgSuccTime", sep=""),
                                                                                                                    pattern="*.csv", full.names=TRUE)
    mstimeout <- t*1000
    lapply(listSelSuccRatePerProver, partial(boxplot1topdf, ylab="Success rate in each configuration (%)",
                                              fontsizex=1, fontsizel=1, fontsizem=1, mary=4, line=3))
    lapply(listSelAvgSuccTimePerProver, partial(boxplot1topdf, ylab="Success rate in each configuration (%)",
                                                  fontsizex=1, fontsizel=1, fontsizem=1, mary=4, line=3))
    lapply(listSelGoodEncSuccRatePerProver, partial(boxplot1topdf, ylab="Success rate in each configuration (%)",
                                                  fontsizex=1, fontsizel=1, fontsizem=1, mary=4, line=3))
    lapply(listSelGoodEncAvgSuccTimePerProver, partial(boxplot1topdf, ylab="Success rate in each configuration (%)",
                                                  fontsizex=1, fontsizel=1, fontsizem=1, mary=4, line=3))
    lapply(listSelSuccRatePerProverPerCategory, partial(boxplot1topdf, ylab="Success rate in each configuration (%)",
                                                  fontsizex=1, fontsizel=1, fontsizem=1, mary=4, line=3))
    lapply(listSelAvgSuccTimePerProverPerCategory, partial(boxplot1topdf, ylab="Success rate in each configuration (%)",
                                                  fontsizex=1, fontsizel=1, fontsizem=1, mary=4, line=3))
    lapply(listSelGoodEncSuccRatePerProverPerCategory, partial(boxplot1topdf, ylab="Success rate in each configuration (%)",
                                                  fontsizex=1, fontsizel=1, fontsizem=1, mary=4, line=3))
    lapply(listSelGoodEncAvgSuccTimePerProverPerCategory, partial(boxplot1topdf, ylab="Success rate in each configuration (%)",
                                                  fontsizex=1, fontsizel=1, fontsizem=1, mary=4, line=3))

    
}

### Boxplots for first paper graph (goal categories overview), for all different timeouts
for (t in c("10s", "30s", "60s", "120s")) {
    boxplot1topdf(paste(layoutpath, "/", t, "/", "Graph1/eprover-successrate_per_goalcategory.csv", sep=""),
                  title="Eprover", ylab="Success rate (%)", xaxislab=c("cex", "ex", "ver", "syn", "test"))
    boxplot1topdf(paste(layoutpath, "/", t, "/", "Graph1/princess-successrate_per_goalcategory.csv", sep=""),
                  title="Princess CASC version", ylab="Success rate (%)", xaxislab=c("cex", "ex", "ver", "syn", "test"))
    boxplot1topdf(paste(layoutpath, "/", t, "/", "Graph1/vampire-4.0-successrate_per_goalcategory.csv", sep=""),
                  title="Vampire 4.0", ylab="Success rate (%)", xaxislab=c("cex", "ex", "ver", "syn", "test"))
    boxplot1topdf(paste(layoutpath, "/", t, "/", "Graph1/vampire-3.0-successrate_per_goalcategory.csv", sep=""),
                  title="Vampire 3.0", ylab="Success rate (%)", xaxislab=c("cex", "ex", "ver", "syn", "test"))
}



### Boxplots for second paper graph (sorting alternatives overview all categories), for all different timeouts
for (t in c("10s", "30s", "60s", "120s")) {
    if (args[1] == "Merged") {
        mergedboxplot1topdf(paste(layoutpath, "/", t, "/", "Graph2/eprover-successrate_per_typingconfiguration.csv", sep=""),
                      title="Eprover", ylab="Success rate (%)", xaxislab=c("t", "g", "e"))
        mergedboxplot1topdf(paste(layoutpath, "/", t, "/", "Graph2/princess-successrate_per_typingconfiguration.csv", sep=""),
                      title="Princess CASC version", ylab="Success rate (%)", xaxislab=c("t", "g", "e"))
        mergedboxplot1topdf(paste(layoutpath, "/", t, "/", "Graph2/vampire-3.0-successrate_per_typingconfiguration.csv", sep=""),
                      title="Vampire 3.0", ylab="Success rate (%)", xaxislab=c("t", "g", "e"))
        mergedboxplot1topdf(paste(layoutpath, "/", t, "/", "Graph2/vampire-4.0-successrate_per_typingconfiguration.csv", sep=""),
                      title="Vampire 4.0", ylab="Success rate (%)", xaxislab=c("t", "g", "e"))
    } else {
         boxplot1topdf(paste(layoutpath, "/", t, "/", "Graph2/eprover-successrate_per_typingconfiguration.csv", sep=""),
                       title="Eprover", ylab="Success rate (%)", xaxislab=c("t", "g", "e"))
         boxplot1topdf(paste(layoutpath, "/", t, "/", "Graph2/princess-successrate_per_typingconfiguration.csv", sep=""),
                       title="Princess CASC version", ylab="Success rate (%)", xaxislab=c("t", "g", "e"))
         boxplot1topdf(paste(layoutpath, "/", t, "/", "Graph2/vampire-3.0-successrate_per_typingconfiguration.csv", sep=""),
                       title="Vampire 3.0", ylab="Success rate (%)", xaxislab=c("t", "g", "e"))
         boxplot1topdf(paste(layoutpath, "/", t, "/", "Graph2/vampire-4.0-successrate_per_typingconfiguration.csv", sep=""),
                       title="Vampire 4.0", ylab="Success rate (%)", xaxislab=c("t", "g", "e"))
    }
}

### Boxplots for third paper graph (variable alternatives overview, counterexample and synthesis), for all different timeouts
for (t in c("10s", "30s", "60s", "120s")) {
    boxplot1topdf(paste(layoutpath, "/", t, "/", "Graph3/eprover-successrate_per_variableconfiguration.csv", sep=""),
                  title="Eprover", ylab="Success rate (%)", xaxislab=c("u", "in", "ne", "np"))
    boxplot1topdf(paste(layoutpath, "/", t, "/", "Graph3/princess-successrate_per_variableconfiguration.csv", sep=""),
                  title="Princess CASC version", ylab="Success rate (%)", xaxislab=c("u", "in", "ne", "np"))
    boxplot1topdf(paste(layoutpath, "/", t, "/", "Graph3/vampire-3.0-successrate_per_variableconfiguration.csv", sep=""),
                  title="Vampire 3.0", ylab="Success rate (%)", xaxislab=c("u", "in", "ne", "np"))
    boxplot1topdf(paste(layoutpath, "/", t, "/", "Graph3/vampire-4.0-successrate_per_variableconfiguration.csv", sep=""),
                  title="Vampire 4.0", ylab="Success rate (%)", xaxislab=c("u", "in", "ne", "np"))
}

### Boxplots for fourth paper graph (simplification alternatives overview), for all different timeouts
for (t in c("10s", "30s", "60s", "120s")) {
    if (args[1] == "Merged") {
        mergedboxplot1topdf(paste(layoutpath, "/", t, "/", "Graph4/eprover-successrate_per_simplificationconfiguration.csv", sep=""),
                                        title="Eprover", ylab="Success rate (%)", xaxislab=c("n", "g", "d"))
        mergedboxplot1topdf(paste(layoutpath, "/", t, "/", "Graph4/princess-successrate_per_simplificationconfiguration.csv", sep=""),
                                        title="Princess CASC version", ylab="Success rate (%)", xaxislab=c("n", "g", "d"))
        mergedboxplot1topdf(paste(layoutpath, "/", t, "/", "Graph4/vampire-3.0-successrate_per_simplificationconfiguration.csv", sep=""),
                                        title="Vampire 3.0", ylab="Success rate (%)", xaxislab=c("n", "g", "d"))
        mergedboxplot1topdf(paste(layoutpath, "/", t, "/", "Graph4/vampire-4.0-successrate_per_simplificationconfiguration.csv", sep=""),
                                        title="Vampire 4.0", ylab="Success rate (%)", xaxislab=c("n", "g", "d"))
    } else {
        boxplot1topdf(paste(layoutpath, "/", t, "/", "Graph4/eprover-successrate_per_simplificationconfiguration.csv", sep=""),
                                        title="Eprover", ylab="Success rate (%)", xaxislab=c("n", "g", "d"))
        boxplot1topdf(paste(layoutpath, "/", t, "/", "Graph4/princess-successrate_per_simplificationconfiguration.csv", sep=""),
                                        title="Princess CASC version", ylab="Success rate (%)", xaxislab=c("n", "g", "d"))
        boxplot1topdf(paste(layoutpath, "/", t, "/", "Graph4/vampire-3.0-successrate_per_simplificationconfiguration.csv", sep=""),
                                        title="Vampire 3.0", ylab="Success rate (%)", xaxislab=c("n", "g", "d"))
        boxplot1topdf(paste(layoutpath, "/", t, "/", "Graph4/vampire-4.0-successrate_per_simplificationconfiguration.csv", sep=""),
                                        title="Vampire 4.0", ylab="Success rate (%)", xaxislab=c("n", "g", "d"))
    }
}

### Boxplots for fifth paper graph (simplification alternatives, timeout-wise, all categories and all provers)
for (t in c("10s", "30s", "60s", "120s")) {
    boxplot1topdf(paste(layoutpath, "/", t, "/", "Graph5/simplificationperformance_allprovers_allcategories.csv", sep=""),
                  title=t, ylab="Success rate (%)", xaxislab=c("n", "g", "d"))
}

### Boxplots for sixth paper graph (performance of all comp strategies, all provers and categories together)
for (t in c("10s", "30s", "60s", "120s")) {
    if (args[1] == "Merged") {
        # Color Array is hardcoded
        # SQL graph + highlighting best (hardcoded)
        boxplot1topdf(paste(layoutpath, "/", t, "/", "Graph6/sql_stratperformance_allprovers_allcategories.csv", sep=""), ylab="Success rate (%)",
                      las=2, fontsizex=1, fontsizel=1, mary=4, line=3, title="SQL", col=c(rep("white", 3), "grey", rep("white", 32)))
        # QL graph + highlighting best (hardcoded)
        boxplot1topdf(paste(layoutpath, "/", t, "/", "Graph6/ql_stratperformance_allprovers_allcategories.csv", sep=""), ylab="Success rate (%)",
                      las=2, fontsizex=1, fontsizel=1, mary=4, line=3, title="QL",
                      col=c(rep("white",2), "grey", rep("white", 2), "grey", rep("white", 20), "grey", rep("white", 2), "grey", rep("white", 6)))
    } else {
        boxplot1topdf(paste(layoutpath, "/", t, "/", "Graph6/stratperformance_allprovers_allcategories.csv", sep=""), ylab="Success rate (%)",
                      las=2, fontsizex=1, fontsizel=1, mary=4, line=3, title=layoutpath)
    }
}
## Boxplots (axiomselection alternatives, for all timeouts)
for (t in c("10s", "30s", "60s", "120s")) {
        boxplot1topdf(paste(layoutpath, "/", "AxiomSelection/PerProver", "/", t, "/", "SuccRate", "/", "eprover-successrate_per_axiomselectionconfiguration.csv", sep=""),
                                        title="Eprover", ylab="Success rate (%)", xaxislab=c("a", "ni", "r", "nir"))
        boxplot1topdf(paste(layoutpath, "/", "AxiomSelection/PerProver", "/", t, "/", "SuccRate", "/", "princess-successrate_per_axiomselectionconfiguration.csv", sep=""),
                                        title="Princess CASC version", ylab="Success rate (%)", xaxislab=c("a", "ni", "r", "nir"))
        boxplot1topdf(paste(layoutpath, "/", "AxiomSelection/PerProver", "/", t, "/", "SuccRate", "/", "vampire-3.0-successrate_per_axiomselectionconfiguration.csv", sep=""),
                                        title="Vampire 3.0", ylab="Success rate (%)", xaxislab=c("a", "ni", "r", "nir"))
        boxplot1topdf(paste(layoutpath, "/", "AxiomSelection/PerProver", "/", t, "/", "SuccRate", "/", "vampire-4.0-successrate_per_axiomselectionconfiguration.csv", sep=""),
                                        title="Vampire 4.0", ylab="Success rate (%)", xaxislab=c("a", "ni", "r", "nir"))

}

### Overview of all individual combinations (like graph for RQ6, but including all axiom selection strategies)
for (t in c("10s", "30s", "60s", "120s")) {
    if (args[1] != "Merged") {
        boxplot1topdf(paste(layoutpath, "/", "AxiomSelection/", t, "/OverviewAll/allstratperformance_allprovers_allcategories.csv", sep=""), ylab="Success rate (%)",
                      las=2, fontsizex=0.3, fontsizel=1, mary=4, line=3)
    }
}
