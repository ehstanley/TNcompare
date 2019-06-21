rm(list=ls())
cat("\014") 

library(tidyverse)
library(cowplot)
best <- read.csv("usgs_prof_test/nitrogen_usgs_best.csv")


#We added flags as part of the process of identifying TN method and QA/QC checking
##A	Includes a required value qualified with “<” and thus cannot/should not be used to determine TN	
##B	Assumed data entry error due to an approximate order-of-magnitude difference in TN-r2, and if decimal place was moved, resultant value would be in the ballpark of the mpv.	
##C	Assumed data entry error- reversed reporting of high and low TN analysis results.	
##D	NO3 >> TN or (NO3 + NH4) >> TN these conditions are not possible in nature, but can’t tell if error is due to lab problem or data entry problem (a variation on “X” flag)	
##E	Assumed data entry error- various- often duplicated data entry for 2 different variables	
##F	NO3 method = IC and no difference between NO3 and NO2NO3 despite a difference in mpv for these 2 parameters	
##G	NO3 method = IC and NO3 only was used to calculate TN-c	
##X	Source of error unknown	

#remove non-viable data
best2 <- best[!(best$Flag.TN.c =="A" | best$Flag.TN.c == "B" | best$Flag.TN.c == "C" | best$Flag.TN.c == "E" | best$Flag.TN.c == "G" | 
                  best$Flag.TN.c == "A, F" | best$Flag.TN.c == "B, F"),]
best3 <- best2[!(best2$Flag.TN.d=="A" | best2$Flag.TN.d == "B" | best2$Flag.TN.d == "C" | best2$Flag.TN.d == "E" | best2$Flag.TN.d == "G"),]

str(best3)
best3$TN_r2 <-as.numeric(best3$TN_r2)

###Fig. 3###
#deviations for TN components
best3$biasTKN <- best3$TKN_r2 - best3$TKN_mpv
best3$biasNO3 <- best3$NO3_r2 - best3$NO3_mpv


#magnitude of the deviations ('bias') relative to the mpv
best3$TN.d.bias.. <- (abs(best3$bias.TN.d/best3$TN_mpv))*100
best3$TN.c.bias.. <- (abs(best3$bias.TN.c/best3$TN_mpv))*100

#calculate MADs for each N form, grouped by SRS_ID
mad_TN_d <- best3 %>% select(SRS_ID, bias.TN.d) %>% drop_na(bias.TN.d) %>%
  group_by(SRS_ID) %>% summarise(mad = median(abs(bias.TN.d-median(bias.TN.d))))

mad_TN_c <- best3 %>% select(SRS_ID, bias.TN.c) %>% drop_na(bias.TN.c) %>%
  group_by(SRS_ID) %>% summarise(mad = median(abs(bias.TN.c-median(bias.TN.c))))

#narrowing down the data for TKN and NO3 just to consider those rows where TN_c 
#was calculated from TKN+NO3
mad_TKN <- best3[!is.na(best3$TN_c),]
mad_TKN <- best3 %>% select(SRS_ID, biasTKN) %>% drop_na(biasTKN) %>%
  group_by(SRS_ID) %>% summarise(mad = median(abs(biasTKN-median(biasTKN))))

mad_NO3 <- best3[!is.na(best3$TN_c),]
mad_NO3 <- best3 %>% select(SRS_ID, biasNO3) %>% drop_na(biasNO3) %>%
  group_by(SRS_ID) %>% summarise(mad = median(abs(biasNO3-median(biasNO3))))

MADs <-full_join(mad_TN_d, mad_TN_c, by = "SRS_ID")
MADs <-full_join(MADs, mad_TKN, by = "SRS_ID")
MADs <-full_join(MADs, mad_NO3, by = "SRS_ID")

names(MADs)<- c("SRS_ID", "TN_d", "TN_c", "TKN", "NO3")
MADs$combined <- MADs$TKN + MADs$NO3


p1 <- ggplot(data = MADs, aes(x=TN_c,y=combined)) + geom_point() +
  xlim(0,0.1) + ylim(0,.1) + 
  geom_abline(slope = 1,intercept = 0) +
  labs(x = expression(MAD[TN-c]), y = expression(MAD[TKN] + MAD[NO[2]*NO[3]*-N])) +  
  theme(aspect.ratio=1)

save_plot(p1, filename = "TN graphs/variancemask2.png",base_height = 4,base_aspect_ratio = 1.2,dpi=300)



###Table S1###
#a few basic stats on total number of analyses, number of labs running the analysis- 
tkn_count <-best[!is.na(best$TKN_r),]
length(unique(tkn_count$Lab))

no3_count <-best[!is.na(best$NO3_r),]
length(unique(no3_count$Lab))

TN_r <-best[!is.na(best$TN_r),]
TN_c <- TN_r[which(TN_r$inferred._TN_method == "c"),]
length(unique(TN_c$Lab))
length(unique(TN_r$Lab))
       
TN_d <- TN_r[which(TN_r$inferred._TN_method == "d"),]
length(unique(TN_d$Lab))


###Fig. S2###
trm <- read.csv("usgs_prof_test/nitrogen_usgs_best_trimmed.csv")
plot(trm$NO3_mpv, trm$bias.TN.d, ylab = "TN-d bias", xlab = expression(paste("NO"["2"],"-NO"["3"],"-N (mg/L)")), cex.lab = 1.25, abline(h=0, col="red"), 
     abline(lm(trm$bias.TN.d~trm$NO3_mpv)))

tiff(filename = "TN graphs/figS2.tiff",
     width = 5,
     height = 5,
     units = "in",res = 300,
     pointsize = 10,
     family="sans",
     compression = "lzw")

par(mfrow = c(2,2),
    oma = c(0,0,0,0),
    mar=c(4.5,4.5,.5,.5))

p1 <- ggplot(data = best3,aes(x=NO3_mpv,y=bias.TN.d)) + 
  geom_point() +
  geom_smooth(method="lm",color="darkblue") +
  geom_hline(yintercept = 0,color="red") + 
  labs(x="",y="TN-d bias (full)") +
  scale_y_continuous(limits = c(-2,2))
p1

p2 <- ggplot(data = best3,aes(x=NO3_mpv,y=bias.TN.c)) + 
  geom_point() +
  geom_smooth(method="lm",color="darkblue") +
  geom_hline(yintercept = 0,color="red") + 
  labs(x="",y="TN-c bias (full)") +
  scale_y_continuous(limits = c(-2,2))
p2

p3 <- ggplot(data = trm,aes(x=NO3_mpv,y=bias.TN.d)) + 
  geom_point() +
  geom_smooth(method="lm",color="darkblue") +
  geom_hline(yintercept = 0,color="red") + 
  labs(x=expression(NO[2]*NO[3]*-N~(mg/L)),y="TN-d bias (trimmed)") +
  scale_y_continuous(limits = c(-2,2))
p3

p4 <- ggplot(data = trm,aes(x=NO3_mpv,y=bias.TN.c)) + 
  geom_point() +
  geom_smooth(method="lm",color="darkblue") +
  geom_hline(yintercept = 0,color="red") + 
  labs(x=expression(NO[2]*NO[3]*-N~(mg/L)),y="TN-c bias (trimmmed)") +
  scale_y_continuous(limits = c(-2,2))
p4

s2_plots <- plot_grid(p1,p2,p3,p4,align = "hv",ncol = 2)
s2_plots

save_plot(filename = "TN graphs/FigS2.png",plot = s2_plots,dpi=300,units="in",base_width = 6.5,base_height = 4.5)

dev.off()

c <- lm(best3$bias.TN.c~best3$NO3_mpv)
d <- lm(best3$bias.TN.d~best3$NO3_mpv)
e <- lm(trm$bias.TN.c~trm$NO3_mpv)
f <- lm(trm$bias.TN.d~trm$NO3_mpv)