#### Generate graphs for interpreting encoding comparison study ####
library(ggplot2)
library(pryr)

### Create various boxplots: ####

# returns string w/o leading or trailing whitespace (from Stackoverflow)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

boxplot1topdf <- function(infile, title="", xlab="", ylab="", ylim=c(0,100), fontsizex=1) {
    outfilesplit <- unlist(strsplit(infile, "[/]"))
    filename <- outfilesplit[2]
    filenamenoending <- substr(filename,1,nchar(filename)-4)
    outputpath <- file.path(outfilesplit[1], "graphs")
    dir.create(outputpath, recursive=TRUE, showWarnings=FALSE)
    fulloutputpath <- paste(outputpath, "/", filenamenoending, ".pdf", sep="")
    indata <- read.csv(infile, header=TRUE, sep=",")
    pdf(fulloutputpath)
    boxplot(indata, data=indata, main=title,
            xlab=xlab, ylab=ylab, ylim=ylim, cex.axis=fontsizex)
    dev.off()
}

# listSuccRatePerProver <- list.files("PerProver/SuccRate", pattern="*.csv", full.names=TRUE)
# listAvgSuccTimePerProver <- list.files("PerProver/AvgSuccTime", pattern="*.csv", full.names=TRUE)

# listSuccRatePerProverPerCategory <- list.files("PerProverPerCategory/SuccRate", pattern="*.csv", full.names=TRUE)
# listAvgSuccTimePerProverPerCategory <- list.files("PerProverPerCategory/AvgSuccTime", pattern="*.csv", full.names=TRUE)

# lapply(listSuccRatePerProver, partial(boxplot1topdf, ylab="Success rate in each configuration (%)"))
# lapply(listAvgSuccTimePerProver, partial(boxplot1topdf, ylab="Average time (ms) for successful proofs"))

# lapply(listSuccRatePerProverPerCategory, partial(boxplot1topdf, ylab="Success rate in each configuration (%)"))
# lapply(listAvgSuccTimePerProverPerCategory, partial(boxplot1topdf, ylab="Average time (ms) for successful proofs"))


### Boxplots for first paper graph (goal categories overview)

boxplot1topdf("Graph1/eprover-successrate_per_goalcategory.csv", title="Eprover", ylab="Success rate (%) for compilation strategy", fontsizex=0.9)
boxplot1topdf("Graph1/vampire-4.0-successrate_per_goalcategory.csv", title="Vampire 4.0", ylab="Success rate (%) for compilation strategy", fontsizex=0.9)
boxplot1topdf("Graph1/vampire-3.0-successrate_per_goalcategory.csv", title="Vampire 3.0", ylab="Success rate (%) for compilation strategy", fontsizex=0.9)

### Boxplots for second paper graph (sorting alternatives overview all categories)
boxplot1topdf("Graph2/eprover-successrate_per_typingconfiguration.csv", title="Eprover", ylab="Success rate (%) for compilation strategy")
boxplot1topdf("Graph2/vampire-3.0-successrate_per_typingconfiguration.csv", title="Vampire 3.0", ylab="Success rate (%) for compilation strategy")
boxplot1topdf("Graph2/vampire-4.0-successrate_per_typingconfiguration.csv", title="Vampire 4.0", ylab="Success rate (%) for compilation strategy")

### Boxplots for third paper graph (variable alternatives overview, counterexample and synthesis)
boxplot1topdf("Graph3/eprover-successrate_per_variableconfiguration.csv", title="Eprover", ylab="Success rate (%) for compilation strategy")
boxplot1topdf("Graph3/vampire-3.0-successrate_per_variableconfiguration.csv", title="Vampire 3.0", ylab="Success rate (%) for compilation strategy")
boxplot1topdf("Graph3/vampire-4.0-successrate_per_variableconfiguration.csv", title="Vampire 4.0", ylab="Success rate (%) for compilation strategy")

### Boxplots for fourth paper graph (variable alternatives overview, proof and test)
boxplot1topdf("Graph4/eprover-successrate_per_variableconfiguration.csv", title="Eprover", ylab="Success rate (%) for compilation strategy")
boxplot1topdf("Graph4/vampire-3.0-successrate_per_variableconfiguration.csv", title="Vampire 3.0", ylab="Success rate (%) for compilation strategy")
boxplot1topdf("Graph4/vampire-4.0-successrate_per_variableconfiguration.csv", title="Vampire 4.0", ylab="Success rate (%) for compilation strategy")

### Boxplots for fifth paper graph (simplification alternatives, overview all categories)
boxplot1topdf("Graph5/eprover-successrate_per_simplificationconfiguration.csv", title="Eprover", ylab="Success rate (%) for compilation strategy")
boxplot1topdf("Graph5/vampire-3.0-successrate_per_simplificationconfiguration.csv", title="Vampire 3.0", ylab="Success rate (%) for compilation strategy")
boxplot1topdf("Graph5/vampire-4.0-successrate_per_simplificationconfiguration.csv", title="Vampire 4.0", ylab="Success rate (%) for compilation strategy")








