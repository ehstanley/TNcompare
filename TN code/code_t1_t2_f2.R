# ===============================================================================================================
#Table 1 and 2

# ======================================================
# Required packages
library(DescTools) #trimming
library(dplyr) #summary

# ======================================================
# Data import and preparation
dataN<-read.csv("data_t1_t2_f2.csv",header=T)


# There are 64 observations that have TN-d but also TKN and NO3
# Use those values of TKN and NO3 to add 64 observations of TN-c
#Determine which are those 64 cases
newobs=dataN[which(dataN$TN_method=="d" & !is.na(dataN$TKN)),]
#Assign them the "calculated" label
newobs$TN_method="c"
#Obtain TN-c
newobs$TN=newobs$TKN+newobs$NO3
#Add the new observations to the data set
dataN<-rbind(dataN,newobs)
rownames(dataN)<-NULL


# Calculate TN deviations and trimmed deviations
dataN$dev=dataN$TN-dataN$TN_mpv

# Obtain trimmed deviations (by method)
#Which values from TNc should be dropped
trimc=attr(Trim(dataN$dev[dataN$TN_method=="c"],trim=.1,na.rm=T),"trim")
dropc=which(dataN$TN_method=="c")[trimc]
#Which values from TNd should be dropped
trimd=attr(Trim(dataN$dev[dataN$TN_method=="d"],trim=.1,na.rm=T),"trim")
dropd=which(dataN$TN_method=="d")[trimd]
#Dropping 20% from TN-c deviations, and 20% from TN-d deviations
dataN$dtrim=NA
dataN$dtrim[-sort(c(dropc,dropd))]=dataN$dev[-sort(c(dropc,dropd))]


# Generate variable of method and concentration
dataN$m_c=as.factor(paste(dataN$TN_method,dataN$concentration))


# ======================================================
# Table 1: Summary of deviations
tb11=summarize(group_by(dataN,category=TN_method), 
               n=length(dev), Mean = round(mean(dev),3),SD=round(sd(dev),3))
tb21=summarize(group_by(dataN,category=m_c), 
               n=length(dev), Mean = round(mean(dev),3),SD=round(sd(dev),3))
tb12=summarize(group_by(dataN,category=TN_method), 
               n=sum(!is.na(dtrim)), Mean = round(mean(dtrim,na.rm=TRUE),3),SD=round(sd(dtrim, na.rm = TRUE),3))
tb22=summarize(group_by(dataN,category=m_c), 
               n=sum(!is.na(dtrim)), Mean = round(mean(dtrim,na.rm=TRUE),3),SD=round(sd(dtrim, na.rm = TRUE),3))
table1=cbind(rbind(tb11,tb21),rbind(tb12,tb22)[,-1])
table1=table1[c(1,2,4,6,3,5),-1]
names(table1)=c("Full_n", "Full_Mean", "Full_SD","Trimmed_n", "Trimmed_Mean", "Trimmed_SD")
row.names(table1)=c("Calculated", "Direct","Calc Low", "Direct Low", "Calc High", "Direct High")
table1


# ======================================================
# Table 2: Variance comparison (F-test p-values)

vart=function(groupvar,group1,group2){
  round(c(var.test(dataN$dev[which(groupvar==group1)],dataN$dev[which(groupvar==group2)])$p.value,
    var.test(dataN$dtrim[which(groupvar==group1)],dataN$dtrim[which(groupvar==group2)])$p.value),3)
}
table2=data.frame(rbind(
vart(dataN$TN_method,"d","c"),
vart(dataN$m_c,"c (low)","d (low)"),
vart(dataN$m_c,"c (high)","d (high)"),
vart(dataN$m_c,"c (low)","c (high)"),
vart(dataN$m_c,"d (low)","d (high)")))
names(table2)=c("Full","Trimmed")
row.names(table2)=c("Calc - Direct","C Low - D Low","C High - D High","C Low - C High","D Low - D High")
table2

# ======================================================
# Obtain p-values from t-tests
#Full data calculated - Direct
t.test(dataN$dev[which(dataN$TN_method=="d")],dataN$dev[which(dataN$TN_method=="c")])$p.value
#Trimmed data calculated - Direct
t.test(dataN$dtrim[which(dataN$TN_method=="d")],dataN$dtrim[which(dataN$TN_method=="c")])$p.value
#Trimmed data low: calculated - Direct
t.test(dataN$dtrim[which(dataN$m_c=="c (low)")],
       dataN$dtrim[which(dataN$m_c=="d (low)")])$p.value
#Trimmed data high: calculated - Direct
t.test(dataN$dtrim[which(dataN$m_c=="c (high)")],
       dataN$dtrim[which(dataN$m_c=="d (high)")],var.equal = TRUE)$p.value

# ======================================================
# Save trimmed data if needed
#write.csv(dataN,file="data_full.csv")
#dataNtrim=dataN[-which(is.na(dataN$dtrim)),]
#write.csv(dataNtrim,file="data_trimmed.csv")



# ===============================================================================================================
#Figure 2

# ======================================================
# Required packages
library(ggplot2)
library(cowplot)

# ======================================================
# Data import and preparation
dataN<-read.csv("data_t1_t2_f2.csv",header=T)

# Obtain information for each SRS (sample size, TN_mpv and MAD)
uSRS=unique(dataN$SRS_ID)
mad=matrix(ncol=2,nrow=length(uSRS))
n=matrix(ncol=2,nrow=length(uSRS))
uTN=c()
for(i in 1:length(uSRS)){
  subs1=which(dataN$SRS_ID==uSRS[i]&dataN$TN_method=="d")
  subs2=which(dataN$SRS_ID==uSRS[i]&dataN$TN_method=="c")
  n[i,1]=length(subs1)
  n[i,2]=length(subs2)
  mad[i,1]=median(abs(dataN$TN_mpv[subs1]-dataN$TN[subs1]))
  mad[i,2]=median(abs(dataN$TN_mpv[subs2]-dataN$TN[subs2]))
  uTN[i]=min(dataN$TN_mpv[which(dataN$SRS_ID==uSRS[i])])
}

# Data with MAD and TN_mpv for each SRS and method
datamad=data.frame(rep(uSRS,2),c(mad[,1],mad[,2]),
                   rep(uTN,2), rep(c("Direct","Calculated"),each=length(uSRS)))
names(datamad)=c("SRS","MAD","uTN","Method")

# ======================================================
# Plots for Figure 2
p1<-ggplot(datamad, aes(x=uTN, y=MAD)) +
  geom_point(aes(x=uTN, y=MAD, shape=Method, color=Method),size=3)+
  theme(legend.title=element_blank(),
        legend.justification = c(0,1),legend.position = c(0,1))+
  labs(x = expression("TN"["MPV"]), y = expression("MAD" ["TN"]))+ 
  scale_color_grey(start=0.1, end=0.4)
p2<-ggplot(datamad, aes(x=Method, y=MAD, fill=Method)) +
  geom_boxplot()+
  labs(y = expression("MAD" ["TN"]))+
  theme(legend.position = "None")+
  scale_fill_grey(start=0.1, end=0.8)
fig2<-plot_grid(p2,p1,labels = c("A","B"),ncol = 2,label_size = 10)
fig2

# If needed, export the plot
save_plot("Fig2.png",plot = fig2,base_height = 3,dpi=300,base_aspect_ratio = 2.3)
