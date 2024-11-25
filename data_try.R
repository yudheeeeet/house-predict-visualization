url <- "https://raw.githubusercontent.com/nbarizki/JabodetabekHousePricing/main/data/refined_data.csv"

# Method 1: Using curl
library(curl)
con <- curl(url)
data <- try(read.csv(con))
close(con)

# Method 2: Direct download
temp <- tempfile()
download.file(url, temp)
data <- read.csv(temp)
unlink(temp)

