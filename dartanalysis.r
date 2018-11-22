library(readr)
library(ggplot2)

dat <- read_csv("/home/ckinneer/research/analysis-code/data.csv")
dat <- dartsweepnocrossover

dat$generation <- as.factor(dat$generation)

p <- ggplot(data=dat, aes(x=generation,y=profit))
p <- p + geom_boxplot()
p + facet_wrap(~ timestep)

p <- p + ylab("Diversity\n(average pairwise tree edit distance)") + xlab("Generation") + scale_fill_discrete(name="Starting Plan")  + theme_bw()
p <- p + theme(text=element_text(size=10), title=element_text(size=30,face="bold"),legend.title=element_text(size=30,face="bold"),legend.text=element_text(size=25),legend.key.size=unit(0.75,"in"))
p + facet_wrap(~ scenario) + scale_color_manual(values=cbPalette,name="Starting Plan")

dartsweep <- read_csv("~/dartsweep.csv")
p <- ggplot(data=dat, aes(x=X11/1000/60,y=X12))
p <- p + geom_point() + annotate("point", x = 355.159/60, y = 4.44, colour = "red") + geom_hline(yintercept=4.44, color = "red")
p + xlab("Time (Minutes)") + ylab("Utility")

# apdaptation
testdata <- read_csv("~/testdata.csv", col_names = FALSE)
data <- testdata

# sum up the runtime
datasum <- ddply(data,.(X8),transform,sumTime = cumsum(X4))

p <- ggplot(data=datasum, aes(x=sumTime/1000/60,y=X5,color=X8))
p <- p + geom_line()
p + xlab("Time (Minutes)") + ylab("Utility") + labs(color = 'Planner')

data$generation <- as.factor(data$X2)
p <- ggplot(data=data, aes(x=X2,y=X5,color=X8))
p <- p + geom_line()
p
