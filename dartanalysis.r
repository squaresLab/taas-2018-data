library(readr)
library(ggplot2)
library("plyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("dplyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")

#MISSING DATA
# nodec2 seed 24
# scratch seed 24

# colorblind color scheme
cbPalette <- c("#47242B","#5A607C", "#3EAA9A", "#C3E270", "#A18E7B")

# start paper graphs
# start ga graphs
# 7.49 4.77
ga <- read_csv("~/research/taas-2018-data/processed/ga.csv")

ga$scenario <- factor(ga$scenario, levels = c("env","nosurv","nodec2","scratch"))
ga$scenario <- revalue(ga$scenario, c("env"="Environment Only", "nodec2"="Slow Descend", "nosurv"="No Survivability", "scratch"="Scratch"))

#ga$generation <- as.factor(ga$generation)
sub <- subset(ga,ga$seed==18 & (ga$popsize==1000 | ga$scenario=='scratch'))
sub <- subset(ga,(ga$popsize==1000 | ga$scenario=='scratch'))

drops <- c("scenario","plan","generation","init")
dataagg <- ga[,!(names(ga) %in% drops)]
dataagg1 <- aggregate(dataagg,by=list(ga$generation,ga$scenario),FUN=mean,na.rm=TRUE)

# sum up the runtime
datasum <- ddply(dataagg1,.(Group.2),transform,sumTime = round(cumsum(runtime)/1000))
# generation graph
p <- ggplot(data=datasum, aes(x=Group.1,y=profit,color=Group.2))
p <- p + theme_bw()
p <- p + theme(text=element_text(size=21), title=element_text(size=21,face="bold"),legend.title=element_text(size=21,face="bold"),legend.text=element_text(size=20),legend.key.size=unit(0.5,"in"))
p <- p + scale_color_manual(values=cbPalette, name="Starting\nPlan")
p <- p + ylab("Utility") + xlab("Generation")
p + geom_line(lwd=1.5)
# runtime graph
p <- ggplot(data=datasum, aes(x=sumTime/60,y=profit,color=Group.2))
p <- p + geom_line() + theme_bw() #+ facet_wrap(~ timestep)
p <- p + theme(text=element_text(size=21), title=element_text(size=21,face="bold"),legend.title=element_text(size=21,face="bold"),legend.text=element_text(size=20),legend.key.size=unit(0.5,"in"))
p <- p + ylab("Utility") + xlab("Planning Time (Minutes)")
p <- p + scale_color_manual(values=cbPalette,name="Starting\nPlan")
p + geom_line(lwd=1.5)
#try to fill in missing data
dat2 <- datasum %>%
  complete(sumTime = full_seq(sumTime, period=1)) %>%
  fill(-sumTime)

# start pladapt graphs (overall run)
pladapt <- read_csv("~/research/taas-2018-data/processed/pladapt/pladapt.csv")

pladapt$scenario <- factor(pladapt$scenario, levels = c("env","nosurv","nodec2","scratch","pmc"))
pladapt$scenario <- revalue(pladapt$scenario, c("env"="Environment\nOnly", "nodec2"="Slow\nDescend", "nosurv"="No\nSurvivability", "scratch"="Scratch", "pmc"="PRISM"))


p <- ggplot(data=pladapt, aes(x=scenario,y=(targets+destoryed)))
p <- p + ylab("Utility") + xlab("Starting Plan") + theme_bw()
p <- p + theme(text=element_text(size=21), title=element_text(size=21,face="bold"),legend.title=element_text(size=21,face="bold"),legend.text=element_text(size=20),legend.key.size=unit(0.5,"in"))
p + geom_boxplot(lwd=1.5)

p <- ggplot(data=pladapt, aes(x=scenario,y=decisionTimeAvg/1000/60))
p <- p + ylab("Avg Decision Time (Minutes)") + xlab("Starting Plan") + theme_bw()
p <- p + theme(text=element_text(size=21), title=element_text(size=21,face="bold"),legend.title=element_text(size=21,face="bold"),legend.text=element_text(size=20),legend.key.size=unit(0.5,"in"))
p + geom_boxplot(lwd=1.5)

# agg by timestep
dataagg1 <- aggregate(dataagg,by=list(ga$generation,ga$scenario,ga$timestep),FUN=mean,na.rm=TRUE)

# sum up the runtime
# 10 6
datasum <- ddply(dataagg1,.(Group.2,Group.3),transform,sumTime = round(cumsum(runtime)/1000))
#datasum <- subset(datasum,datasum$timestep < 39)
# downshift the values in the last timestep to make the rest of the graph more readable
datasum$profit[datasum$timestep==39] = datasum$profit[datasum$timestep==39] - 4.5
# generation graph
p <- ggplot(data=datasum, aes(x=Group.1,y=profit,color=Group.2))
p <- p + theme_bw()
p <- p + theme(text=element_text(size=10), legend.position=c(.875,.025), title=element_text(size=21,face="bold"),legend.title=element_text(size=21,face="bold"),legend.text=element_text(size=20),legend.key.size=unit(0.25,"in"))
p <- p + scale_color_manual(values=cbPalette, name="Starting Plan")
p <- p + ylab("Utility") + xlab("Generation")
p + geom_line(lwd=1.5) + facet_wrap(~ Group.3) + theme(strip.text.x = element_text(size = 8,margin = margin(.1,0,.1,0,"cm")))
# runtime graph
p <- ggplot(data=datasum, aes(x=sumTime/60,y=profit,color=Group.2))
p <- p + geom_line() + theme_bw() #+ facet_wrap(~ timestep)
p <- p + theme(text=element_text(size=10), legend.position=c(.875,.025), title=element_text(size=21,face="bold"),legend.title=element_text(size=21,face="bold"),legend.text=element_text(size=20),legend.key.size=unit(0.25,"in"))
p <- p + ylab("Utility") + xlab("Planning Time (Minutes)")
p <- p + scale_color_manual(values=cbPalette,name="Starting Plan")
p + geom_line(lwd=1.5) + facet_wrap(~ Group.3) + theme(strip.text.x = element_text(size = 8,margin = margin(.1,0,.1,0,"cm")))# + ylim(0,5)

#param sweep
dartsweep <- read_csv("~/research/taas-2018-data/dartsweepnocrossover.csv", col_names=FALSE)
dartsweep$planner = "GP"
dartsweep <- dartsweep[,11:13]
dartsweep[nrow(dartsweep)+1,] = list(355.159*1000,4.44,"PRISM")

mycolours <- c("PRISM" = "black", "GP" = "grey")

p <- ggplot(data=dartsweep, aes(x=X11/1000/60,y=X12,color=planner))
p <- p + theme_bw() 
p <- p + theme(text=element_text(size=20),legend.position=c(.8,.5), title=element_text(size=21,face="bold"),legend.title=element_text(size=21,face="bold"),legend.text=element_text(size=20),legend.key.size=unit(0.5,"in"))
p <- p + geom_point(size=4) + geom_hline(yintercept=4.44, color = "black")
p + xlab("Time (Minutes)") + ylab("Utility") + scale_color_manual("Planner", values = mycolours)

# a graph to explain the team's transit
# colorblind color scheme
cbPalette <- c("#47242B","#5A607C", "#3EAA9A", "#C3E270", "#A18E7B")
cbPalette <- c("#47242B", "#C3E270")

dart <- read_csv("~/research/taas-2018-data/dart.csv")
dartenv <- read_csv("~/research/taas-2018-data/dartenv.csv")
dartenv <- subset(dartenv,dartenv$Environment!="none")
p <- ggplot(data=dart, aes(x=Timestep,y=Altitude))
p <- p + theme_bw()
p <- p + scale_color_manual(values = cbPalette)
p <- p + theme(text=element_text(size=20),legend.position=c(.8,.62), title=element_text(size=21,face="bold"),legend.title=element_text(size=21,face="bold"),legend.text=element_text(size=20),legend.key.size=unit(0.5,"in"))
p + geom_point(aes(color=Formation),size=4) + geom_point(data=dartenv,aes(x=Timestep,y=Altitude, shape=Environment),size=4)  + scale_shape_manual(values=c(15, 17, 18))

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
