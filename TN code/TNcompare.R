rm(list=ls())
library(tidyverse)
# library(maps)
# library(MethComp)
# library(deming)
library(mcr)
library(lubridate)
library(cowplot)
library(gridExtra)
library(grid)

###Fig. 5 related###
lagosN <- read_csv("TN data/lagosN.csv") #Concurrent measurements of TN, TKN, and NO2NO3 extracted from LAGOS v1.087.2

nrow(lagosN)
table(lagosN$tn_censorcode)
table(lagosN$no2no3_censorcode)
table(lagosN$tkn_censorcode)

n_lakes = length(unique(lagosN$lagoslakeid))
n_programs = length(unique(lagosN$programname))

dat_cen <- lagosN %>%   
  filter(!grepl('LE', tn_censorcode)) %>% 
  filter(grepl('LE', no2no3_censorcode) | grepl('LE', tkn_censorcode)) 
dat_not_cen <- lagosN %>% 
  filter(!grepl('LE', tn_censorcode)) %>% 
  filter(!grepl('LE', no2no3_censorcode)) %>% 
  filter(!grepl('LE', tkn_censorcode)) 

#Statistical Comparison of two approaches 
set.seed(539892)                  
comp_dat <- dat_not_cen %>% group_by(programname,lagoslakeid) %>% 
  slice(sample(x = n(),size = 1))

ggplot(data = comp_dat, aes(x=tn_d,y=tn_c)) + geom_point()
x <- (comp_dat$tn_d)
y <- (comp_dat$tn_c)
m2 <- mcreg(x,y,method.reg="PaBa")
MCResult.plot(x=m2, add.legend=TRUE,equal.axis=TRUE,xn=50,ci.area = TRUE,x.lab="TN_d (mg/L)",y.lab = "TN_c (mg/L)")
getCoefficients(m2)


# Code to generate Figure 5

pred2 <- m2@glob.coef[1] + m2@glob.coef[2]*x
MSE = mean((y-pred2)^2)
resids <- MCResult.getResiduals(m2)
resids$tn <- x
p4 <- ggplot(data = resids,aes(x=tn,y=optimized)) + geom_point(alpha = 0.6) + 
  labs(x="TN-d (mg/L)",y="Orthogonal Residual Value") + 
  geom_hline(yintercept = 0)
p4

resids$no2no3 <- comp_dat$no2no3
p5 <- ggplot(data = resids,aes(x=no2no3,y=optimized)) + geom_point(alpha = 0.6) + 
  labs(x=expression(NO[2]*NO[3]*-N (mg/L)),y="Orthogonal Residual Value") + 
  geom_hline(yintercept = 0)
p5


p3 <- ggplot(data = comp_dat,aes(x=tn_d,y=tn_c))  + geom_abline(slope = 1,intercept = 0,linetype=2,col="lightgrey") + 
  geom_abline(slope = 1.02546866,intercept = -0.03037493,linetype=1) +
  geom_point(alpha=0.6) +
  labs(x="TN-d (mg/L)",y="TN-c (mg/L)") + 
  coord_cartesian(xlim=c(0,13),ylim=c(0,13))
p3

plots_stats <- plot_grid(p3,p4,p5, labels = c("A","B","C"),ncol = 3,label_size = 10)
plots_stats
save_plot("TN graphs/PaBA_2.png",plot = plots_stats,base_height = 3,dpi=300,base_aspect_ratio = 3)