
library(dplyr)
library(plotly)
library(tidyr)

##want to make a figure for DLs with ALL lagos data, not just what gets exported. Noah sent the new 1.087.2. version to do this:

#setwd("~/Downloads")
#lagosN<-read.csv("lagosvalues_eventidb_data.csv", header=T)

#save data to project as RDS for future reference

#setwd("~/Documents/LocalGitProjects/N_ADHD")
#saveRDS(lagosN, file = "TN data/LAGOS10872forDL.rds")

lagosN<-readRDS("TN data/LAGOS10872forDL.rds")

Nvars<-c("tn", "tkn", "no2no3")

lagosNvars<-lagosN[lagosN$tmpvarshortname %in% Nvars,]

cctn<-lagosNvars[lagosNvars$tmpvarshortname == "tn",]
summary(cctn$censorcode)
cctkn<-lagosNvars[lagosNvars$tmpvarshortname == "tkn",]
summary(cctkn$censorcode)
ccno3<-lagosNvars[lagosNvars$tmpvarshortname == "no2no3",]
summary(ccno3$censorcode)

NsDLs<-lagosNvars[,c("lagoslakeid", "programid", "tmpvarshortname", "datavalue", "detectionlimit", "date")]

NsDLs$date<-as.Date(NsDLs$date, '%Y-%m-%d')

NsDLs$sampleyear<-as.numeric(format(NsDLs$date, '%Y'))

NsDLs$date<-NULL

noearly<-NsDLs[NsDLs$sampleyear>1979,]
noearly$programid<-as.factor(noearly$programid)

tndat<-noearly[noearly$tmpvarshortname=="tn",]
tndat.c<-tndat[!with(tndat, is.na(tmpvarshortname)),]
tnprogmed<- tndat.c %>% group_by(programid) %>% summarise(dlmed=median(detectionlimit, na.rm=T),
                                                          med=median(datavalue, na.rm=T),
                                                          count=n())
tnprogmed$var="TN"

tkndat<-noearly[noearly$tmpvarshortname=="tkn",]
tkndat.c<-tkndat[!with(tkndat, is.na(tmpvarshortname)),]
tknprogmed<- tkndat.c %>% group_by(programid) %>% summarise(dlmed=median(detectionlimit, na.rm=T),
                                                            med=median(datavalue, na.rm=T),
                                                            count=n())
tknprogmed$var="TKN"

no3dat<-noearly[noearly$tmpvarshortname=="no2no3",]
no3dat.c<-no3dat[!with(no3dat, is.na(tmpvarshortname)),]
no3progmed<- no3dat.c %>% group_by(programid) %>% summarise(dlmed=median(detectionlimit, na.rm=T),
                                                              med=median(datavalue, na.rm=T),
                                                              count=n())
no3progmed$var="NO2NO3"

progmedsall<-rbind(tnprogmed, tknprogmed, no3progmed)
                                                   

no3dat.c$DLchar<-as.character(no3dat.c$detectionlimit)
no3alldls<-na.omit(unique(no3dat.c$detectionlimit))
variable<-"NO2NO3"
no3dlframe<-data.frame(no3alldls, variable)
names(no3dlframe)<-c("DL", "variable")

tndat.c$DLchar<-as.character(tndat.c$detectionlimit)
tnalldls<-na.omit(unique(tndat.c$detectionlimit))
variable<-"TN"
tndlframe<-data.frame(tnalldls, variable)
names(tndlframe)<-c("DL", "variable")

tkndat.c$DLchar<-as.character(tkndat.c$detectionlimit)
tknalldls<-na.omit(unique(tkndat.c$detectionlimit))
variable<-"TKN"
tkndlframe<-data.frame(tknalldls, variable)
names(tkndlframe)<-c("DL", "variable")

dlsnoprogall<-rbind(no3dlframe, tndlframe, tkndlframe)

colors="grey70"

f1 <- list(
  family = "Arial, sans-serif",
  size = 14,
  color = "black"
)

f2<- list(
  family = "Arial, sans-serif",
  size=12,
  color="black"
)


a<-list(
  title = 'Detection Limit (mg/L)',
  titlefont = f1,
  showticklabels = TRUE,
  ticktext = list('0.00001', '0.0001', '0.001', '0.01', '0.1', '1'),
  tickvals = list(.01, .1, 1, 10, 100, 1000),
  tickfont = f2,
  exponentformat = "E",
  showline=TRUE,
  linecolor=toRGB("black"),
  linewidth=2,
  zeroline=FALSE,
  showgrid=FALSE,
  type="log"
)

nitnit=TeX("NO_{2}NO_{3}-N")

a2<-list(
  title = FALSE,
  showticklabels = TRUE,
  ticktext=list("NO NO -N", "TN", "TKN"),
  tickvals=list(0,1,2),
  tickfont = f2,
  exponentformat = "E",
  showline=TRUE,
  linecolor=toRGB("black"),
  linewidth=2,
  zeroline=FALSE,
  showgrid=FALSE
)


j <- plot_ly(dlsnoprogall, x = ~variable, y = ~DL, type = 'scatter', mode = 'markers', width=288, height=288, color = ~variable, colors=colors,
             #Choosing the range of the bubbles' sizes:
             marker = list(size= 10, opacity = 0.5, sizemode = 'diameter', line=list(width=2, color=toRGB("black")))) %>%
  layout(title = '',
         xaxis = a2,
         yaxis = a,
         showlegend = FALSE) %>%
         config(mathjax='cdn')
j
orca(j, "TN graphs/DLBubblesnosize_log.png", scale= 20,width= 288, height = 288)

api_create(j, filename="dl_nosize_log")




#####program med scaled size by program below but rejected for ms

q <- plot_ly(progmedsall, x = ~var, y = ~dlmed, text = ~programid, type = 'scatter', mode = 'markers', size = ~count, color = ~var, colors=colors,
             #Choosing the range of the bubbles' sizes:
             sizes = c(10, 50),
             marker = list(opacity = 0.5, sizemode = 'diameter', line=list(width=2, color='black'))) %>%
  layout(title = '',
         xaxis = list(showgrid = FALSE, title="Nitrogen Variable"),
         yaxis = list(showgrid = TRUE, title='Program Median Detection Limit'),
         showlegend = FALSE)


orca(q, "TN graphs/DLBubbles.png")
#chart_link = api_create(q, filename="dlbyvar")
