Root_Path <- 'C:/zhao088/Desk20210304/Manuscript.eco-evo/Mark tessa/github Oct12/ciliates'
OPath <- paste(Root_Path,sep='')
setwd(OPath)

library("scales")
library('tidyverse') 
library("lme4")
library("lmerTest")
library("nlme")

data <- read.csv('DIVERCE_TdB_Ciliates_Traits.csv')
head(data)
unique(data$ID_spec)

# part 1.1 #
# mean trait correlation,Spiro_C
data2 <-subset(data,ID_spec=="Spiro_C")

cor(data2$mean_ar, data2$mean_area, method = c("pearson"))  #-0.4396676
cor(data2$mean_ar, data2$mean_speed, method = c("pearson")) #0.277062
cor(data2$mean_ar, data2$mean_linearity, method = c("pearson")) #0.04867956

cor(data2$mean_area, data2$mean_speed, method = c("pearson")) #0.3048742
cor(data2$mean_area, data2$mean_linearity, method = c("pearson")) #0.3357897

cor(data2$mean_speed, data2$mean_linearity, method = c("pearson")) #0.6075996
a2<-summary(lm(data2$mean_linearity~data2$mean_speed))
a2

#
library("ggpubr")
library("ggplot2")
a<-ggscatter(data2, x = "mean_speed", y = "mean_linearity", color = "Treatment",
             shape = 19, size=3,
             add = "reg.line", conf.int = FALSE,
             cor.coef = TRUE, cor.method = "pearson",
             title= "",  
             xlab = "mean_speed", ylab = "mean_linearity"
)

a+theme_bw()+
  #geom_abline(intercept =a2$coefficients[1,1], slope = a2$coefficients[2,1], color="black", linetype="solid", size=1)+
  theme(plot.title = element_text(hjust = 0.5,size=15), legend.position='right')
  #geom_ab



# part 1.2 #
# mean trait correlation,Spiro_D
unique(data$ID_spec)
data2 <-subset(data,ID_spec=="Spiro_D")

cor(data2$mean_ar, data2$mean_area, method = c("pearson"))  #
cor(data2$mean_ar, data2$mean_speed, method = c("pearson")) #
cor(data2$mean_ar, data2$mean_linearity, method = c("pearson")) #

cor(data2$mean_area, data2$mean_speed, method = c("pearson")) #
cor(data2$mean_area, data2$mean_linearity, method = c("pearson")) #

cor(data2$mean_speed, data2$mean_linearity, method = c("pearson")) #0.2919668
a2<-summary(lm(data2$mean_linearity~data2$mean_speed))
a2

#
library("ggpubr")
library("ggplot2")
a<-ggscatter(data2, x = "mean_speed", y = "mean_linearity", color = "Treatment",
             shape = 19, size=3,
             add = "reg.line", conf.int = FALSE,
             cor.coef = TRUE, cor.method = "pearson",
             title= "",  
             xlab = "mean_speed", ylab = "mean_linearity"
)

a+theme_bw()+
  #geom_abline(intercept =a2$coefficients[1,1], slope = a2$coefficients[2,1], color="black", linetype="solid", size=1)+
  theme(plot.title = element_text(hjust = 0.5,size=15), legend.position='right')
#geom_ab



# part 1.3 #
# mean trait correlation,Tetra_1
unique(data$ID_spec)
data2 <-subset(data,ID_spec=="Tetra_1")

cor(data2$mean_ar, data2$mean_area, method = c("pearson"))  #-0.728053
cor(data2$mean_ar, data2$mean_speed, method = c("pearson")) #0.6130398
cor(data2$mean_ar, data2$mean_linearity, method = c("pearson")) #

cor(data2$mean_area, data2$mean_speed, method = c("pearson")) #-0.6111685
cor(data2$mean_area, data2$mean_linearity, method = c("pearson")) #

cor(data2$mean_speed, data2$mean_linearity, method = c("pearson")) #0.6492907
a2<-summary(lm(data2$mean_linearity~data2$mean_speed))
a2





# mean_ar VS mean_area
library("ggpubr")
library("ggplot2")
a<-ggscatter(data2, x = "mean_ar", y = "mean_area", color = "Treatment",
             shape = 19, size=3,
             add = "reg.line", conf.int = FALSE,
             cor.coef = TRUE, cor.method = "pearson",
             title= "",  
             xlab = "mean_speed", ylab = "mean_linearity"
)

a+theme_bw()+
   theme(plot.title = element_text(hjust = 0.5,size=15), legend.position='right')

# mean_ar VS mean_speed
library("ggpubr")
library("ggplot2")
a<-ggscatter(data2, x = "mean_ar", y = "mean_speed", color = "Treatment",
             shape = 19, size=3,
             add = "reg.line", conf.int = FALSE,
             cor.coef = TRUE, cor.method = "pearson",
             title= "",  
             xlab = "mean_speed", ylab = "mean_linearity"
)

a+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size=15), legend.position='right')

# mean_area VS mean_speed
library("ggpubr")
library("ggplot2")
a<-ggscatter(data2, x = "mean_area", y = "mean_speed", color = "Treatment",
             shape = 19, size=3,
             add = "reg.line", conf.int = FALSE,
             cor.coef = TRUE, cor.method = "pearson",
             title= "",  
             xlab = "mean_speed", ylab = "mean_linearity"
)

a+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size=15), legend.position='right')


# mean_speed VS mean_linearity
library("ggpubr")
library("ggplot2")
a<-ggscatter(data2, x = "mean_speed", y = "mean_linearity", color = "Treatment",
             shape = 19, size=3,
             add = "reg.line", conf.int = FALSE,
             cor.coef = TRUE, cor.method = "pearson",
             title= "",  
             xlab = "mean_speed", ylab = "mean_linearity"
)

a+theme_bw()+
 theme(plot.title = element_text(hjust = 0.5,size=15), legend.position='right')



# part 1.4 #
# mean trait correlation,Tetra_2
unique(data$ID_spec)
data2 <-subset(data,ID_spec=="Tetra_2")

cor(data2$mean_ar, data2$mean_area, method = c("pearson"))  #
cor(data2$mean_ar, data2$mean_speed, method = c("pearson")) #0.5534801
cor(data2$mean_ar, data2$mean_linearity, method = c("pearson")) #

cor(data2$mean_area, data2$mean_speed, method = c("pearson")) #
cor(data2$mean_area, data2$mean_linearity, method = c("pearson")) #-0.5321342

cor(data2$mean_speed, data2$mean_linearity, method = c("pearson")) #0.6031208
a2<-summary(lm(data2$mean_linearity~data2$mean_speed))
a2


# mean_ar VS mean_speed
library("ggpubr")
library("ggplot2")
a<-ggscatter(data2, x = "mean_ar", y = "mean_speed", color = "Treatment",
             shape = 19, size=3,
             add = "reg.line", conf.int = FALSE,
             cor.coef = TRUE, cor.method = "pearson",
             title= "",  
             xlab = "mean_speed", ylab = "mean_linearity"
)

a+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size=15), legend.position='right')

# mean_area VS mean_linearity
library("ggpubr")
library("ggplot2")
a<-ggscatter(data2, x = "mean_area", y = "mean_linearity", color = "Treatment",
             shape = 19, size=3,
             add = "reg.line", conf.int = FALSE,
             cor.coef = TRUE, cor.method = "pearson",
             title= "",  
             xlab = "mean_area", ylab = "mean_linearity"
)

a+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size=15), legend.position='right')


# mean_speed VS mean_linearity
library("ggpubr")
library("ggplot2")
a<-ggscatter(data2, x = "mean_speed", y = "mean_linearity", color = "Treatment",
             shape = 19, size=3,
             add = "reg.line", conf.int = FALSE,
             cor.coef = TRUE, cor.method = "pearson",
             title= "",  
             xlab = "mean_speed", ylab = "mean_linearity"
)

a+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size=15), legend.position='right')




# part 1.5 #
# mean trait correlation,Para_4
unique(data$ID_spec)
data2 <-subset(data,ID_spec=="Para_4")

cor(data2$mean_ar, data2$mean_area, method = c("pearson"))  #
cor(data2$mean_ar, data2$mean_speed, method = c("pearson")) #
cor(data2$mean_ar, data2$mean_linearity, method = c("pearson")) #

cor(data2$mean_area, data2$mean_speed, method = c("pearson")) #
cor(data2$mean_area, data2$mean_linearity, method = c("pearson")) #

cor(data2$mean_speed, data2$mean_linearity, method = c("pearson")) #0.5464877
a2<-summary(lm(data2$mean_linearity~data2$mean_speed))
a2


# mean_speed VS mean_linearity
library("ggpubr")
library("ggplot2")
a<-ggscatter(data2, x = "mean_speed", y = "mean_linearity", color = "Treatment",
             shape = 19, size=3,
             add = "reg.line", conf.int = FALSE,
             cor.coef = TRUE, cor.method = "pearson",
             title= "",  
             xlab = "mean_speed", ylab = "mean_linearity"
)

a+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size=15), legend.position='right')




