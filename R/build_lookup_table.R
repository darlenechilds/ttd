# ==========================================================
# Function: build_lookup_table()
# Project: ttd 
# for each year compute/predict f12 conc.  

# ==========================================================
build_lookup_table <- function(year, tracer, ratio = 1.8){
  
  C0_fun <- switch(tracer,
                   CFC12 = cO_fun_xeff_f12,
                   SF6   = cO_fun_xeff_sf6)
  
  Gamma_seq <- seq(1, 200, by = 0.5)
  Delta_seq <- ratio * Gamma_seq
  
  tracer_mod <- sapply(seq_along(Gamma_seq), function(i) {
    compute_Cxt(
      t = year,
      Gamma = Gamma_seq[i],
      Delta = Delta_seq[i],
      C0_fun = C0_fun
    )
  })
  
  lookup <- data.frame(
    Gamma = Gamma_seq,
    Delta = Delta_seq,
    tracer_mod
  )
  
  names(lookup)[3] <- tracer
  
  lookup
}






