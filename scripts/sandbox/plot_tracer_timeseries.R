# plot tracer timeseries for different water masses

rm(list = ls())
library(oce)

d <- read.csv("data/OCADS_tracers_o2.csv")    #OCADS data
#filter out good data, 2 = good, 6 = mean of duplicates
d <- d[d$cfc12_flag==2 | d$cfc12_flag ==6,]

unique(d$cfc12_flag)



d$theta <- swTheta(e$CTDSAL.y,e$CTDTMP.y,e$CTDPRS.y,referencePressure = 0)
d$sigma2 <- swSigma2(e$CTDSAL.y,e$theta,e$CTDPRS.y)

  



neadw <- e[which(e$LATITUDE.y > 56 & e$LONGITUDE.y <60),]
  neadw <- neadw[which(neadw$sigma2 > 36.965 & neadw$sigma2 < 37.04),]

  neadw_avesf6_ocads <- mean(neadw$SF6.x)
  neadw_sdsf6_ocads <- sd(neadw$SF6.x)
  neadw_avef12_ocads <- mean(neadw$CFC12.x)
  neadw_sdf12_ocads <- sd(neadw$CFC12.x)
  
  neadw_avesf6_unsub <- mean(neadw$SF6.y)
  neadw_sdsf6_unsub <- sd(neadw$SF6.y)
  neadw_avef12_unsub <- mean(neadw$CFC12.y)
  neadw_sdf12_unsub <- sd(neadw$CFC12.y)

  e_neadw <- c(i,neadw_avesf6_ocads,neadw_sdsf6_ocads,neadw_avef12_ocads,neadw_sdf12_ocads,
                           neadw_avesf6_unsub,neadw_sdsf6_unsub,neadw_avef12_unsub,neadw_sdf12_unsub)
s <- as.data.frame(rbind(s,e_neadw))
}



