setwd('~/public_git/oecd_pisa_data')
library(readxl)
oecd_pisa_data <- read_excel("oecd_pisa_data.xlsx")


library(countrycode)
oecd_pisa_data$iso3c <- countrycode(oecd_pisa_data$Jurisdiction, "country.name", "iso3c")

# Country classification by income
wb_classification <- read_excel("wb_hist_country_class_by_income.xlsx")
require(reshape2)
wb_classification <- melt(wb_classification, 
                          id.vars = c('iso3c','country'),
                          variable.name = 'Year',
                          value.name = "wb_class")

oecd_pisa_data <- merge(oecd_pisa_data, 
                        wb_classification[,c('iso3c','Year','wb_class')], 
                        by = c('iso3c','Year'),
                        all.x = TRUE, all.y = FALSE)

save(oecd_pisa_data, file  = 'oecd_pisa_data.RData')
