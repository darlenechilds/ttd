# ==========================================================
# Function: estimate_gamma()
# Project: ttd 
# for each year and tracer compute/predict gamma (mean age)  

# ==========================================================


estimate_gamma <- function(d, tracer) {
  
  s <- NULL
  
  for (yr in unique(d$yr)) {
    
    e <- d[d$yr == yr, ]
    lookup <- build_lookup_table(yr, tracer)
    
    e$Gamma <- sapply(e[[tracer]], function(obs) {
      lookup$Gamma[which.min(abs(lookup[[tracer]] - obs))]
    })
    
    s <- rbind(s, e)
  }
  
  s
}