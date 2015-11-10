### EVA November 2015 ####
#### MutualKissing How Many Events? ###


library(doBy)
library(gmodels)
library(MASS)
library(ggplot2)
library(Hmisc)
library(lme4)
library(car)
library(languageR)
library(car)
library(xtable)

#this is the dataset in which we asked for general events -- i.e., how many events
rm(list=ls())
#read data in
getwd()
#setwd("/Users/evinapatata/GitHub/Reciprocals/MutualKissing/2_HowManyEvents")
data <- read.csv("FormattedHowMany.csv")
#drop columns
str(data)
#kick out NAs
data$sentence <- NULL
length(complete.cases(data))
completecases <- na.omit(data)

#kick out fillers
pairs <-completecases[ ! completecases$event %in% c("filler"), ]
pairs = droplevels(subset(pairs, eventCat!='filler'))
pairs = droplevels(subset(pairs, construction!='filler'))
str(pairs)


#means by condition
conditiononlymeans <- summaryBy(count ~ eventCat + construction , FUN=c(mean,sd), data= pairs)
conditiononlymeans

#means by item
itemmeans <- summaryBy(count ~ event + construction , FUN=c(mean,sd), data= pairs)
itemmeans

## densityplots
library(lattice)
with(subset(pairs,log10(count)<20), densityplot(~log10(count) | factor(paste(construction,eventCat)),layout=c(3,2)))

pairs$log.how.many <- log(pairs$count+1)
#means by condition
conditiononlymeans <- summaryBy(log.how.many ~ eventCat + construction , FUN=c(mean,sd), data= pairs)
conditiononlymeans

#### GRAPHING ####
y.ticks <- c(1,1.5,2,2.5,3, 3.5,4)
Construction <- factor(pairs$construction)

bar <- ggplot(pairs, aes(x=eventCat,y=log(count+1), fill = Construction, ))
bar + theme_bw() + stat_summary(fun.y= mean, geom = "bar", colour="black", position = "dodge") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width= 0.4) +
  scale_fill_manual(values=c("#253494", "#2ca25f"),name="Construction")  + 
  scale_colour_manual(values=c("grey50", "grey80", "black")) + 
  theme(axis.text.y = element_text(size=16), 
        axis.text.x = element_text(size=16),
        strip.text.x = element_text(size=24),
        axis.title.y = element_text(size=24),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=24))+
  labs(x="", y="Event Counts, on log scale", fill="Construction")+
  #scale_y_continuous(breaks=c(0.0,.5,1,1.5), labels = c("0","~1.6","~2.7","~4.5"))+
  scale_y_continuous(breaks=log(y.ticks), labels = y.ticks)+
#  facet_wrap(~eventCat, scales ="free")
  ggsave("EventCountsBar.pdf", width=12, height=8, unit='in')

bar <- ggplot(pairs, aes(x=Construction,y=log(count+1), fill = Construction, ))
bar + theme_bw() + stat_summary(fun.y= mean, geom = "bar", colour="black", position = "dodge") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width= 0.4) +
  scale_fill_manual(values=c("#253494", "#2ca25f"),name="Construction")  + 
  scale_colour_manual(values=c("grey50", "grey80", "black")) + 
  theme(axis.text.y = element_text(size=16), 
        axis.text.x = element_text(size=16),
        strip.text.x = element_text(size=24),
        axis.title.y = element_text(size=24),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=24))+
  labs(x="", y="Event Counts, on log scale", fill="Construction")+
  #scale_y_continuous(breaks=c(0.0,.5,1,1.5), labels = c("0","~1.6","~2.7","~4.5"))+
  scale_y_continuous(breaks=log(y.ticks), labels = y.ticks)+
  #  facet_wrap(~eventCat, scales ="free")
  ggsave("ConstructionCountBar.pdf", width=12, height=8, unit='in')


pairbar <- ggplot(pairs, aes(x=event,y=log(count+1), fill = factor(construction)))
pairbar + 
  stat_summary(fun.y= mean, geom = "bar", colour="black", position = "dodge") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width= 0.4) + 
  scale_fill_manual(values=c("#9ecae1", "#3182bd", "#08519c"),name="Construction")  + 
  scale_colour_manual(values=c("grey50", "grey80", "black")) + 
  labs(x="Events", y="log(Event Counts)", fill="Construction") +
  names(completecases$event)+
  theme(axis.text.x = element_text(angle = 90,size=14), 
  axis.text.y = element_text(size=14))+
  facet_wrap(~eventCat,scales="free_x")
ggsave("PairEventsCountBar.pdf", width=18, height=9)

#######CONTINUE WITH STATS##############
#TEST FOR MAIN EFFECT -- ROGER's PAPER
#construction is now named category because I was too lazy to rewrite everything!
pairs$category <- as.factor(pairs$construction)
pairs$category.numeric <- sapply(pairs$construction,function(i) contr.sum(2)[i,])

pairs$eventCat <- as.factor(pairs$eventCat)
within(pairs, {
  eventCat.numeric <- C(eventCat, helmert)
  print(attributes(eventCat.numeric))
})
str(pairs)
#eventCat.numeric
pairs$eventCat1 <- eventCat.numeric[1,]
pairs$eventCat2 <- eventCat.numeric[2,]
str(pairs)

#model with full effect structure
m.full <- lmer(log.how.many ~ 1 + category.numeric*(eventCat1+eventCat2) + (1|workerId) + (0+ category.numeric|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId) + (0 + category.numeric:eventCat1|workerId) + (0+category.numeric:eventCat2|workerId) + (1 | event) + (0+ category.numeric|event), data=pairs,REML=F)
system.time(m.noEventCat <- lmer(log.how.many ~ 1 + category.numeric + category.numeric:(eventCat1+eventCat2) + (1|workerId) + (0+ category.numeric|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId) + (0 + category.numeric:eventCat1|workerId) + (0+category.numeric:eventCat2|workerId) + (1 | event) + (0+ category.numeric|event), data=pairs,REML=F))
anova(m.noEventCat,m.full)
system.time(m.noCategory <- lmer(log.how.many ~ 1 + eventCat1+eventCat2 + category.numeric:(eventCat1+eventCat2) + (1|workerId) + (0+ category.numeric|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId) + (0 + category.numeric:eventCat1|workerId) + (0+category.numeric:eventCat2|workerId) + (1 | event) + (0+ category.numeric|event), data=pairs,REML=F))
anova(m.noCategory,m.full)
system.time(m.noInteraction <- lmer(log.how.many ~ 1 + eventCat1+eventCat2 + category.numeric + (1|workerId) + (0+ category.numeric|workerId) +(0+eventCat1|workerId) + (0+eventCat2|workerId) + (0 + category.numeric:eventCat1|workerId) + (0+category.numeric:eventCat2|workerId) + (1 | event) + (0+ category.numeric|event), data=pairs,REML=F))
anova(m.noInteraction,m.full)

