library(tidyverse)
dat <- read_csv("TN data/unfeas.csv") #nitrogen data extracted from lagos v1.087.2

dat <- dat %>% mutate(tnc=tkn+no2no3) %>% 
  mutate(methoddiff=tn-tnc)
dat_equal <- dat %>% filter(methoddiff==0 | tn==tkn)
dat_unfeas <- dat %>% filter(!eventidb_10872 %in% dat_equal$eventidb_10872)
ratios <- dat_unfeas %>% 
  mutate(tkn_tn = tkn/tn) %>% 
  mutate(nh4_tkn = nh4/tkn) %>% 
  mutate(no3_tn = no2no3/tn) %>% 
  mutate(nh4_tn = nh4/tn) %>% 
  select(-tn,-tkn,-no2no3,-nh4,-sampledepth_lagos,-date,-lagoslakeid,-tnc,-methoddiff) %>% 
  gather(key = ratio,value = value,-eventidb_10872,-programname) %>% 
  drop_na()

ratios$ratio <- as.factor(ratios$ratio)
levels(ratios$ratio)
levels(ratios$ratio) <- c("NH[4]*':'*TKN","NH[4]*':'*TN","NO[2]*NO[3]*':'*TN","TKN*':'*TN")
ggplot(data = ratios,aes(x=value+0.001)) + geom_histogram(bins=500) + facet_wrap(vars(ratio),scales = "free",labeller = label_parsed)+ 
  theme(axis.line=element_line()) +
  scale_x_log10(breaks = c(0.001,0.01,0.1,1,10,100),labels=c("0.001","0.01","0.1","1","10","100"),limits=c(0.0008,100)) + scale_y_log10(limits=c(1,1000)) + 
  geom_vline(xintercept = 1.0,colour="red") + labs(x="Ratio Value + 0.001") +
  theme(strip.background = element_blank())
ggsave("TN graphs/unfeas.png",width = 6.5,height = 4,units = "in",dpi = 300)