setwd('~/public_git/oecd_pisa_data')
load('oecd_pisa_data.RData')

oecd_pisa_data <- subset(oecd_pisa_data, wb_class == "H")

require(dplyr)
oecd_pisa_data <- 
  oecd_pisa_data %>%
  group_by(Year) %>%
  mutate(annual_highincome_mean = mean(Average, na.rm = T))

oecd_pisa_data$of_mean <- oecd_pisa_data$Average / oecd_pisa_data$annual_highincome_mean

require(reshape2)
math_read_df <- dcast(subset(oecd_pisa_data, Variable != 'science'), Year + iso3c ~ Variable, value.var = 'of_mean')

years <- unique(math_read_df$Year)
years <- years[order(years)]

math_read_start_end <- data.frame()
for (i in 2:length(years)) {
  start_yr <- subset(math_read_df, Year == years[i-1])
  names(start_yr)[3:4] <- paste0("start_", names(start_yr)[3:4])
  end_yr <- subset(math_read_df, Year == years[i])
  names(end_yr)[3:4] <- paste0("end_", names(end_yr)[3:4])
  math_read_start_end <-
    rbind(math_read_start_end, 
          merge(end_yr, start_yr[,c("iso3c","start_mathematics","start_reading")], 
                by = 'iso3c'))
}




