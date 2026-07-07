# ==========================================================
# Function: read_ocads()
# Project: ttd
# Purpose:
# Read all OCADS hydrographic files, standardize variable
# names, and return one combined data frame.
#
# Returns:
#   tibble containing all OCADS observations
# ==========================================================


#function to read in ocad data
read_ocads <- function(url) {
  tryCatch({
    
    # Read lines 
    lines <- readLines(url)
    
    # Find the header - EXPOCODE
    header_line <- grep("^EXPOCODE", lines)[1]
    
    # Read from that line
    df <- read.csv(text = lines[header_line:length(lines)], stringsAsFactors = FALSE)
    
    df$source <- url
    
    return(df)
    
  }, error = function(e) {
    message(paste("Failed:", url))
    return(NULL)
  })
}
