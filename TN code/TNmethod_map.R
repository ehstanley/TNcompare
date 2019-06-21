library(maps)
library(dplyr)
#map of distribution of lakes showing TN-c vs TN-d distribution using data derived from 
#State of the lakes project. As of 2 Dec, SoL_data.rds is not on the TN_ADHD git repo, 
#but it is on ehstanley/SoL/SoL_data,along with lakes.rds.
#Because they're in a different repo, I've commented them out below.

sol <-readRDS("TN data/SoL_data.rds")
lakes <- readRDS("TN data/lakes.rds")
lake1 <- lakes[, 1:3]

sol2 <-left_join(sol, lake1, by = "lagoslakeid")
sol_TNc <-sol2[!is.na(sol2$tn_calculated),]
sol_TNc <-sol_TNc[!duplicated(sol_TNc$lagoslakeid),]
sTNc <- sol_TNc[, 39:40]

sol_TNd <-sol2[!is.na(sol2$tn),]
sol_TNd <-sol_TNd[!duplicated(sol_TNd$lagoslakeid),]
sTNd <- sol_TNd[, 39:40]

png(filename = "TN graphs/TN_method_map.png")
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE)
points(sTNc$nhd_long, sTNc$nhd_lat, cex = 0.1, pch=20, col ="darkgray")
points(sTNd$nhd_long, sTNd$nhd_lat, cex = 0.1, pch = 20, col = "blue")
dev.off()
