# merge tracer data with oxygen data (UNSUB)
rm(list = ls())
library(dplyr)
library(oce)

#load data
d <- read.csv("data/UNSUB_tracers.csv")   #read in tracer data
o2 <- read.csv("data/UNSUB_oxygen.csv")   # O2 data
o2 <- o2[,-c(1,2)]  # remove index columns

# clean up o2 data
# remove nas
o2 <- o2[!is.na(o2$OXYGEN_umolperkg), ]

# deal with duplicates, look at stdev to ensure dups are fine
dups <- o2[duplicated(o2$SAMPNO) | duplicated(o2$SAMPNO, fromLast = TRUE), ]
ind <- which(o2$SAMPNO %in% dups$SAMPNO)  

# compute mean + sd for o2 per SAMPNO
stats <- dups %>%
  group_by(SAMPNO) %>%
  summarise(
    OXYGEN_mlperl_mean = mean(OXYGEN_mlperl, na.rm = TRUE),
    OXYGEN_mlperl_sd   = sd(OXYGEN_mlperl, na.rm = TRUE),
    OXYGEN_umolperkg_mean = mean(OXYGEN_umolperkg, na.rm = TRUE),
    OXYGEN_umolperkg_sd   = sd(OXYGEN_umolperkg, na.rm = TRUE)
  )

# old way
# stats <- aggregate(OXYGEN_mlperl ~ SAMPNO, data = dups,
#                    FUN = function(x) c(mean = mean(x, na.rm = TRUE),
#                                        sd   = sd(x, na.rm = TRUE)))
# stats$o2_mean <- stats$OXYGEN_mlperl[, "mean"]
# stats$o2_sd   <- stats$OXYGEN_mlperl[, "sd"]
# stats$OXYGEN_mlperl <- NULL

# use sd to find good and non-good duplicates
o2_needslookinginto <- stats[which(stats$OXYGEN_mlperl_sd>1),]  #look at dups closer
o2_good <- stats[which(stats$OXYGEN_mlperl_sd<1),]   #keep means of duplicates

# go back a take a look at dups in main data file
ind1 <- which(o2$SAMPNO %in% o2_needslookinginto$SAMPNO)  # extract data dups from orig
o2_needslookinginto <- o2[ind1,]  
o2_removeo2lessthan1 <-  o2_needslookinginto[-which(o2_needslookinginto$OXYGEN_mlperl<1),]    # remove bad measurements
o2_keep_1st <- o2_removeo2lessthan1[!duplicated(o2_removeo2lessthan1$SAMPNO,fromLast = TRUE),]    #keep first duplicate measurement, as these were inspected manually
o2_keep_1st <- o2_keep_1st %>%
  select("STNNBR","SAMPNO", "CTDPRS","CTDTMP", "CTDSAL","OXYGEN_mlperl","OXYGEN_umolperkg")

# merge good mean data with manually inspected data
# get meta data back in 
o2_good_merge <- merge(o2_good,o2,by = "SAMPNO", all.x = TRUE)
o2_good_merge <- o2_good_merge[!duplicated(o2_good_merge$SAMPNO), ]
o2_good_merge <- o2_good_merge %>%
  select("STNNBR","SAMPNO", "CTDPRS","CTDTMP", "CTDSAL","OXYGEN_mlperl_mean","OXYGEN_umolperkg_mean")
colnames(o2_good_merge) <- c("STNNBR","SAMPNO", "CTDPRS","CTDTMP", "CTDSAL","OXYGEN_mlperl","OXYGEN_umolperkg")

# add O2_keep_1st
o2_good_all <- rbind(o2_good_merge,o2_keep_1st)

#re-merge o2_good_all with orig dataset
o2 <- o2[-ind,]  # delete dups so dont get dups when remerging
o2_final <- rbind(o2,o2_good_all)

write.csv(o2_final,"data/UNSUB_oxygen_clean.csv")


