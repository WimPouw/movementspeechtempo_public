#http://www.sthda.com/english/articles/32-r-graphics-essentials/132-plot-grouped-data-box-plot-bar-plot-and-more/
#https://www.datanovia.com/en/blog/how-to-create-a-beautiful-plots-in-r-with-summary-statistics-labels/

library(lme4)
library(lmerTest)
library(ggplot2)
library(emmeans)
library(tibble)
library(dplyr)
library(ggpubr) 
library(tidyverse)
library(rstatix)
library("ggExtra")
library(performance)
library(multcomp)
library(emmeans)

#### Different folders ####
basefolder <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(basefolder)

#### Read data for envelope 8 Hz ####
read.csv(paste0(basefolder, "rhythm_dataset_for_stat_env8.csv"), h=TRUE, sep=",")->data
summary(data)

#### summary function for plots ####
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}



## Pre-processing of data structure ##
data$speaker<-as.factor(data$speaker)
data$condition_m<-as.factor(data$condition_m)
data$condition_s<-as.factor(data$condition_s)
data$mov_av_rhythm<-as.numeric(data$mov_av_rhythm)

data$condition_s->data$tasks
levels(data$tasks)[levels(data$tasks)=='spont speech1'] <- 'spontaneous speech'
levels(data$tasks)[levels(data$tasks)=='spont speech2'] <- 'spontaneous speech'
levels(data$tasks)[levels(data$tasks)=='spont speech3'] <- 'spontaneous speech'
levels(data$tasks)[levels(data$tasks)=='reading1'] <- 'read speech'
levels(data$tasks)[levels(data$tasks)=='reading2'] <- 'read speech'
levels(data$tasks)[levels(data$tasks)=='reading3'] <- 'read speech'

data$condition_s->data$trial
levels(data$trial)[levels(data$trial)=='reading1'] <- '1'
levels(data$trial)[levels(data$trial)=='reading2'] <- '2'
levels(data$trial)[levels(data$trial)=='reading3'] <- '3'
levels(data$trial)[levels(data$trial)=='spont speech1'] <- '1'
levels(data$trial)[levels(data$trial)=='spont speech2'] <- '2'
levels(data$trial)[levels(data$trial)=='spont speech3'] <- '3'
levels(data$trial)[levels(data$trial)=='motion only'] <- '1'
data$trial<-as.factor(data$trial)

summary(data)
str(data)

##################################################
#### Pedaling rate ###############################
##################################################

#### PREPROCESS: remove everything above 3 standard deviations and speaker ilu (spurious markers)####
movdata<- data[ which(data$condition_m != 'speech only' & data$speaker!='ilu'),]  
uppermov<-mean(movdata$mov_av_rhythm) + (sd(movdata$mov_av_rhythm) * 3)
movdata2 <- subset(movdata, mov_av_rhythm<uppermov)
hist(movdata2$mov_av_rhythm, breaks=10)
summary(movdata2)

#### PLOT PEDALING ####
levels(movdata2$tasks)[levels(movdata2$tasks)=='motion only'] <- 'no speech'
png("./figures/PedalingRates.png", units="in", width=6, height=4, res=900)
e <- ggplot(movdata2, aes(x = condition_m, y = mov_av_rhythm))
e + theme_bw()+facet_wrap(~tasks, ncol=3)+geom_violin(aes(fill = condition_m), trim = FALSE) + 
  geom_boxplot(width = 0.2)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  theme(legend.position = "none")+ylab("Leg-cycling rate in Hz")+xlab("Workload")
dev.off()

#### Write descriptive statistics in table PEDALING ####
datamov <- data_summary(movdata2, varname="mov_av_rhythm", 
                      groupnames=c("condition_m", "tasks"))
write.table(datamov, "means/Means.csv", row.names=FALSE, sep=",")


#### LMER PEDALING ####
movdata2$condition_m<-factor(movdata2$condition_m, levels=c("speech only", "low ", "moderate "))
# with interaction
mot1<-lmer(mov_av_rhythm~condition_m*tasks + trial+ (1+condition_m*tasks+trial|speaker), data = movdata2)
summary(mot1)
# performance library to check some general model assumptions

f1<-check_model(mot1) # high collinearity for interactive model - for this reason additive model


# final model without interaction
mot2<-lmer(mov_av_rhythm~condition_m+tasks + trial+(1+condition_m+tasks|speaker), data = movdata2)
summary(mot2)
check_model(mot2)

# write output table with coefficients of the model
coeffs <- coef(summary(mot2)) # get estimates, etc.
p <- pnorm(abs(coeffs[, "t value"]), lower.tail = FALSE) * 2 # add the much disputed p-values
coeffmot2 <- cbind(coeffs, "p value" = round(p,3)) # combine it into one object
write.csv(coeffmot2, "modelcoef/coeffmot2.csv") # export


##################################################
#### Speech rates ################################
##################################################

#### PREPROCESS: remove everything above and below 3 sd

speechdata<- data[ which(data$tasks != 'motion only'),]
upper<-mean(speechdata$env8_av_rhythm) + (sd(speechdata$env8_av_rhythm) * 3)
lower<-mean(speechdata$env8_av_rhythm) - (sd(speechdata$env8_av_rhythm) * 3)
upperimf1<-mean(speechdata$env8_av_rhythmimf1) + (sd(speechdata$env8_av_rhythmimf1) * 3)
lowerimf1<-mean(speechdata$env8_av_rhythmimf1) - (sd(speechdata$env8_av_rhythmimf1) * 3)
upperimf2<-mean(speechdata$env8_av_rhythmimf2) + (sd(speechdata$env8_av_rhythmimf2) * 3)
lowerimf2<-mean(speechdata$env8_av_rhythmimf2) - (sd(speechdata$env8_av_rhythmimf2) * 3)

speechdata2<- speechdata[ which(speechdata$env8_av_rhythm<upper & speechdata$env8_av_rhythm>lower),]
speechdataimf1<- speechdata[ which(speechdata$env8_av_rhythmimf1<upperimf1 & speechdata$env8_av_rhythmimf1>lowerimf1),]
speechdataimf2<- speechdata[ which(speechdata$env8_av_rhythmimf2<upperimf2 & speechdata$env8_av_rhythmimf2>lowerimf2),]

# change "speech only" to "rest"
levels(speechdata2$condition_m)[levels(speechdata2$condition_m)=='speech only'] <- 'rest'
levels(speechdataimf1$condition_m)[levels(speechdataimf1$condition_m)=='speech only'] <- 'rest'
levels(speechdataimf2$condition_m)[levels(speechdataimf2$condition_m)=='speech only'] <- 'rest'

# set reference level to "rest"
speechdata2$condition_m<-factor(speechdata2$condition_m, levels=c("rest", "low ", "moderate "))
speechdataimf1$condition_m<-factor(speechdataimf1$condition_m, levels=c("rest", "low ", "moderate "))
speechdataimf2$condition_m<-factor(speechdataimf2$condition_m, levels=c("rest", "low ", "moderate "))

#### PLOT Speechrates ####

png("./figures/SpeechRate.png", units="in", width=4, height=6, res=900)
f <- ggplot(speechdata2, aes(x = condition_m, y = env8_av_rhythm))+ theme_bw()+facet_wrap(~tasks, ncol=3)+geom_violin(aes(fill = condition_m), trim = FALSE) + 
  geom_boxplot(width = 0.2)+ylim(2.5,6)+
  scale_fill_manual(values = c("#FC4E07","#00AFBB", "#E7B800" ))+
  theme(legend.position = "none")+ylab("Envelope in Hz")+xlab("")

fa <- ggplot(speechdataimf1, aes(x = condition_m, y = env8_av_rhythmimf1)) + theme_bw()+facet_wrap(~tasks, ncol=3)+geom_violin(aes(fill = condition_m), trim = FALSE) + 
  geom_boxplot(width = 0.2)+ylim(2.5,7)+
  scale_fill_manual(values = c("#FC4E07","#00AFBB", "#E7B800"))+
  theme(legend.position = "none")+ylab("IMF1 in Hz")+xlab("")

fb <- ggplot(speechdataimf2, aes(x = condition_m, y = env8_av_rhythmimf2))+ theme_bw()+facet_wrap(~tasks, ncol=3)+geom_violin(aes(fill = condition_m), trim = FALSE) + 
  geom_boxplot(width = 0.2)+ylim(1,4)+
  scale_fill_manual(values = c("#FC4E07","#00AFBB", "#E7B800"))+
  theme(legend.position = "none")+ylab("IMF2 in Hz")+xlab("Workload")

figure <- ggarrange(f, fa, fb,
                    ncol = 1, nrow = 3)
figure

dev.off()


#### LMER Speech rates ####
# final model env8
lmer(env8_av_rhythm~condition_m*tasks+(0+condition_m*tasks|speaker), data=speechdata2)->sp8
summary(sp8)
check_model(sp8)

# write model coefficients
coeffs <- coef(summary(sp8)) # get estimates, etc...
p <- pnorm(abs(coeffs[, "t value"]), lower.tail = FALSE) * 2 # add the much disputed p-values
coeffsp8 <- cbind(coeffs, "p value" = round(p,4)) # combine it into one object
write.csv(coeffsp8, "modelcoef/coeffsp8.csv") # export

# write descriptive stats
dataspeech<-data_summary(speechdata2, varname="env8_av_rhythm", 
                         groupnames=c("condition_m", "tasks"))
write.table(dataspeech, "means/Env8Means.csv", row.names=FALSE, sep=",")


# final model imf1
lmer(env8_av_rhythmimf1~condition_m+tasks+trial+(1+condition_m+tasks+trial|speaker), data=speechdataimf1)->sp8a
summary(sp8a)
check_model(sp8a)

# write model coefficients
coeffs <- coef(summary(sp8a)) # get estimates, etc...
p <- pnorm(abs(coeffs[, "t value"]), lower.tail = FALSE) * 2 # add the much disputed p-values
coeffsp8a <- cbind(coeffs, "p value" = round(p,4)) # combine it into one object
write.csv(coeffsp8a, "modelcoef/coeffsp8a.csv") # export

# write descriptive stats
dataimf1<-data_summary(speechdata, varname="env8_av_rhythmimf1", 
                       groupnames=c("condition_m", "tasks"))
write.table(dataimf1, "means/Envimf1Means.csv", row.names=FALSE, sep=",")

# final model imf2
lmer(env8_av_rhythmimf2~condition_m*tasks+trial+(1+condition_m*tasks|speaker), data=speechdataimf2)->sp8b
summary(sp8b)
check_model(sp8b)

coeffs <- coef(summary(sp8b)) # get estimates, etc...
p <- pnorm(abs(coeffs[, "t value"]), lower.tail = FALSE) * 2 # add the much disputed p-values
coeffsp8b <- cbind(coeffs, "p value" = round(p,4)) # combine it into one object
write.csv(coeffsp8b, "modelcoef/coeffsp8b.csv") # export

# write descriptive stats
dataimf2<-data_summary(speechdata, varname="env8_av_rhythmimf2", 
                       groupnames=c("condition_m", "tasks"))
write.table(dataimf1, "means/Envimf2Means.csv", row.names=FALSE, sep=",")



#################################################
##############   Regression models


Corrdata<- speechdata2[ which(speechdata2$condition_m != 'rest'),]
Corrdataimf1<- speechdataimf1[ which(speechdataimf1$condition_m != 'rest'),]
Corrdataimf2<- speechdataimf2[ which(speechdataimf2$condition_m != 'rest'),]


lmer(env8_av_rhythm~scale(mov_av_rhythm)+tasks+condition_m+trial+(1+mov_av_rhythm+tasks+condition_m|speaker), data=Corrdata)->mod3
summary(mod3)
check_model(mod3)

lmer(env8_av_rhythmimf1~scale(mov_av_rhythm)+condition_m+tasks+trial+(1+mov_av_rhythm+tasks+condition_m|speaker), data=speechdataimf1)->mod3a
summary(mod3a)
check_model(mod3a)

lmer(env8_av_rhythmimf2~scale(mov_av_rhythm)+tasks+condition_m+trial+(1+mov_av_rhythm+tasks+condition_m|speaker), data=speechdataimf2)->mod3b
summary(mod3b)
check_model(mod3b)

### plot
k<-ggplot(Corrdata, aes(x=(mov_av_rhythm), y=env8_av_rhythm)) + 
  geom_point()+ geom_smooth(method = "lm", se=TRUE)+
  stat_regline_equation(label.y = 5.7, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 5.4, aes(label = ..rr.label..))+
  geom_point()+ylab("Envelope in Hz")+xlab("Leg-cycling rate in Hz")+
  scale_color_manual(values = c("#00AFBB","#E7B800"))+
  theme_bw()+theme(legend.position = "none")+facet_wrap(~tasks*condition_m)

k1<-ggplot(Corrdataimf1, aes(x=(mov_av_rhythm), y=env8_av_rhythmimf1)) + 
  geom_point()+ geom_smooth(method = "lm", se=TRUE)+
  stat_regline_equation(label.y = 6.5, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 6.2, aes(label = ..rr.label..))+
  ylab("IMF1 in Hz")+xlab("Leg-cycling rate in Hz")+
  scale_color_manual(values = c("#00AFBB","#E7B800"))+
  theme_bw()+theme(legend.position = "none")+facet_wrap(~tasks*condition_m)

names(Corrdataimf2)[names(Corrdataimf2)=="condition_m"]  <- "Workload"
png("./figures/Correlations.png", units="in", width=10, height=6, res=900)
k2<-ggplot(Corrdataimf2, aes(x=(mov_av_rhythm), y=env8_av_rhythmimf2)) + 
  geom_point()+ylab("IMF2 in Hz")+xlab("Motion rate in Hz")+geom_smooth(method = "lm", se=TRUE)+
  stat_regline_equation(label.y = 3.5, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 3.2, aes(label = ..rr.label..))+
  ylab("IMF2 in Hz")+xlab("Leg-cycling rate in Hz")+
  scale_color_manual(values = c("#00AFBB","#E7B800"))+theme_bw()+theme(legend.position="bottom")+
  theme_bw()+theme(legend.position = "none")+facet_wrap(~tasks*Workload)


figure <- ggarrange(k, k1, k2,
                    ncol = 3, nrow = 1)
figure



