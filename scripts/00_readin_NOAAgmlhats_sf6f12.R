#read in data from NOAA GML website; https://gml.noaa.gov/ccgg/
rm(list = ls())
library(dplyr)

sf6_atm_monthly_means <- read.csv("https://gml.noaa.gov/webdata/ccgg/trends/sf6/sf6_mm_gl.csv",comment.char = "#")

f12_global <- read.csv("https://gml.noaa.gov/aftp/data/hats/cfcs/cfc12/combined/HATS_global_F12.txt",
                         comment.char = "#",header = TRUE,sep = "")

f12_history <- read.csv("https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0164584/CFC_ATM_Hist_2022/Tracer_atmospheric_histories_revised_2023_Table1.csv", header = T)
f12_history <- f12_history[-c(1:2),]

# history from Rigby, M., Mühle, J., Miller, B. R., Prinn, R. G., Krummel, P. B., Steele, L. P., Fraser, P. J., Salameh, P. K., Harth, C. M., Weiss, R. F., Greally, B. R., O'Doherty, S., Simmonds, P. G., Vollmer, M. K., Reimann, S., Kim, J., Kim, K.-R., Wang, H. J., Olivier, J. G. J., Dlugokencky, E. J., Dutton, G. S., Hall, B. D., and Elkins, J. W.: History of atmospheric SF6 from 1973 to 2008, Atmos. Chem. Phys., 10, 10305–10320, https://doi.org/10.5194/acp-10-10305-2010, 2010.
sf6_history <- read.table(text = "
Year Emissions Uncertainty N30_90 N0_30 S0_30 S90_30
1970 0.73 0.08 0.29 0.28 0.27 0.27
1971 0.85 0.09 0.34 0.32 0.29 0.29
1972 0.91 0.10 0.37 0.35 0.33 0.32
1973 1.06 0.11 0.41 0.39 0.36 0.36
1974 1.11 0.12 0.46 0.44 0.40 0.40
1975 1.32 0.14 0.51 0.49 0.45 0.44
1976 1.59 0.17 0.57 0.55 0.50 0.49
1977 1.79 0.19 0.65 0.62 0.56 0.55
1978 1.98 0.21 0.72 0.69 0.63 0.62
1979 2.39 0.24 0.82 0.78 0.71 0.70
1980 2.57 0.26 0.92 0.88 0.81 0.79
1981 2.73 0.27 1.03 0.99 0.91 0.89
1982 2.98 0.29 1.15 1.10 1.01 0.99
1983 3.06 0.30 1.28 1.22 1.13 1.11
1984 3.50 0.34 1.42 1.35 1.25 1.23
1985 3.85 0.37 1.57 1.50 1.39 1.36
1986 4.10 0.39 1.73 1.66 1.54 1.51
1987 4.32 0.41 1.91 1.83 1.70 1.67
1988 4.56 0.43 2.10 2.00 1.87 1.84
1989 4.98 0.49 2.30 2.19 2.04 2.01
1990 5.08 0.49 2.51 2.39 2.23 2.20
1991 5.42 0.55 2.72 2.60 2.43 2.39
1992 5.61 0.56 2.95 2.82 2.64 2.60
1993 5.68 0.55 3.18 3.04 2.86 2.82
1994 5.94 0.57 3.42 3.27 3.08 3.04
1995 6.34 0.58 3.67 3.51 3.31 3.26
1996 6.13 0.60 3.92 3.75 3.55 3.50
1997 5.97 0.58 4.14 3.98 3.79 3.75
1998 5.44 0.58 4.32 4.21 4.02 3.98
1999 5.23 0.58 4.53 4.40 4.23 4.20
2000 5.07 0.58 4.71 4.59 4.44 4.40
2001 5.00 0.57 4.89 4.78 4.63 4.60
2002 5.48 0.59 5.10 4.99 4.83 4.80
2003 6.05 0.57 5.35 5.23 5.05 5.01
2004 5.76 0.54 5.56 5.45 5.28 5.24
2005 6.36 0.52 5.80 5.69 5.51 5.47
2006 6.50 0.50 6.07 5.94 5.75 5.71
2007 7.14 0.53 6.34 6.22 6.01 5.96
2008 7.42 0.63 6.65 6.51 6.29 6.24
", header = TRUE)

f12 <- f12_global$HATS_NH_F12/100
plot(sf6_history$Year,sf6_history$N30_90, xlim = c(1969,2030), ylim = c(0,13), type = "l")
points(sf6_atm_monthly_means$year,sf6_atm_monthly_means$average, type = "l", col = "red")
points(f12_global$HATS_F12_YYYY,f12,type = "l", col = "blue")

tail(sf6_atm_monthly_means)

