lookup <- build_lookup_table(2020, "CFC12")

head(lookup)

head(lookup$tracer)

obs <- d_f12$CFC12[15]

lookup$Gamma[which.min(abs(lookup$tracer - obs))]


lookup <- build_lookup_table(2019, "CFC12")
names(lookup)
which.min(lookup[["CFC12"]])
