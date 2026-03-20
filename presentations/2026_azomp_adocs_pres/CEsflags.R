#text from CEs notes
# (list = ls())


txt <- c(
  "515657 AR7W12 4 m? higher lower down",
  "515760 AR7W15 50m?",
  "515848 NROI001 30 m (low ta)",
  "515895 NROI002 20m (low ta)",
  "What about NROI03? Lower than 1,2, and 4, but should still see this deviation?",
  "516020 NROI004 25 m (high TIC, low ta)",
  "516427 CROI003 40 m – low here",
  "516491 AR7W07 30 m – low here, should be 08?",
  "516536 AR7W04 20m",
  "516604 SROI30 m – TA only",
  "516607 SROI 10m – TA only",
  "516608 SROI 3 m  - TA only",
  "516551 AR7W03 100m – low ta and tic"
)


d <- data.frame(original_text = txt)


d <- data.frame(
  line = seq_along(txt),
  text = txt,
  stringsAsFactors = FALSE
)

library(dplyr)
library(stringr)

d2 <- d %>%
  mutate(
    sample_id = str_extract(text, "^\\d+"),
    site = str_extract(text, "(AR7W\\d+|NROI\\d+|CROI\\d+|SROI\\d+)"),
    depth_m = as.numeric(str_extract(text, "\\d+(?= ?m)")),
    notes = str_replace(text, "^[0-9]+\\s+\\S+\\s+\\d+ ?m[?–-]?", "")
  )

d2$site
#get flag id
flag_id <- d2$sample_id
flag_id <- na.omit(flag_id)
flag_id <- as.numeric(flag_id)




