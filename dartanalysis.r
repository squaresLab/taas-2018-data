library(readr)
library(ggplot2)
library("plyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("dplyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")

#MISSING DATA
# nodec2 seed 24
# scratch seed 24

# start paper graphs
# start ga graphs
ga <- read_csv("~/research/taas-2018-data/processed/ga.csv")
#ga$generation <- as.factor(ga$generation)
sub <- subset(ga,ga$seed==18 & (ga$popsize==1000 | ga$scenario=='scratch'))
sub <- subset(ga,(ga$popsize==1000 | ga$scenario=='scratch'))

drops <- c("scenario","plan","generation","init")
dataagg <- ga[,!(names(ga) %in% drops)]
dataagg1 <- aggregate(dataagg,by=list(ga$generation,ga$scenario),FUN=mean,na.rm=TRUE)

# sum up the runtime
datasum <- ddply(dataagg1,.(Group.2),transform,sumTime = round(cumsum(runtime)/60))
# generation graph
p <- ggplot(data=datasum, aes(x=Group.1,y=profit,color=Group.2))
p + geom_line() #+ facet_wrap(~ timestep)
# runtime graph
p <- ggplot(data=datasum, aes(x=sumTime,y=profit,color=Group.2))
p + geom_line() #+ facet_wrap(~ timestep)

#try to fill in missing data
dat2 <- datasum %>%
  complete(sumTime = full_seq(sumTime, period=1)) %>%
  fill(-sumTime)

# start pladapt graphs (overall run)
pladapt <- read_csv("~/research/taas-2018-data/processed/pladapt/pladapt.csv")
p <- ggplot(data=pladapt, aes(x=scenario,y=(targets+destoryed)))
p + geom_boxplot()

p <- ggplot(data=pladapt, aes(x=scenario,y=decisionTimeAvg/1000/60))
p + geom_boxplot()

# start prelim graphs
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
