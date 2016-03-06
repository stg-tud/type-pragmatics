#### Generate graphs for interpreting encoding comparison study ####
library(ggplot2)

### 1) Create boxplots: For each prover, success rates per goal category ####

boxplot1topdf <- function(infile, outfile, title) {
    indata <- read.csv(infile, header=TRUE, sep=",")
    pdf(outfile)
    boxplot(indata, data=indata, main=title,
            xlab="Goal Category", ylab="Success rate in each configuration")
    dev.off()
}

boxplot1topdf("vampire-proof-4.0-successrate_per_category.csv",
              "graphs/vampire-proof-4.0-successrate_per_category.pdf",
              "Success rate per goal category, Vampire-4.0")
boxplot1topdf("vampire-proof-3.0-successrate_per_category.csv",
              "graphs/vampire-proof-3.0-successrate_per_category.pdf",
              "Success rate per goal category, Vampire-3.0")
boxplot1topdf("eprover-successrate_per_category.csv",
              "graphs/eprover-successrate_per_category.pdf",
              "Success rate per goal category, Eprover")
boxplot1topdf("princess-casc-successrate_per_category.csv",
              "graphs/princess-casc-successrate_per_category.pdf",
              "Success rate per goal category, Princess-casc")


### 2) Create boxplots: For each prover, avg success time per goal category ####

boxplot2topdf <- function(infile, outfile, title) {
    indata <- read.csv(infile, header=TRUE, sep=",")
    pdf(outfile)
    boxplot(indata, data=indata, main=title,
            xlab="Goal Category", ylab="Average success time in each configuration")
    dev.off()
}

boxplot2topdf("vampire-proof-4.0-avgsuccesstime_per_category.csv",
              "graphs/vampire-proof-4.0-avgsuccesstime_per_category.pdf",
              "Average success time per goal category, Vampire-4.0")
boxplot2topdf("vampire-proof-3.0-avgsuccesstime_per_category.csv",
              "graphs/vampire-proof-3.0-avgsuccesstime_per_category.pdf",
              "Average success time per goal category, Vampire-3.0")
boxplot2topdf("eprover-avgsuccesstime_per_category.csv",
              "graphs/eprover-avgsuccesstime_per_category.pdf",
              "Average success time per goal category, Eprover")
boxplot2topdf("princess-casc-avgsuccesstime_per_category.csv",
              "graphs/princess-casc-avgsuccesstime_per_category.pdf",
              "Average success time per goal category, Princess-casc")





