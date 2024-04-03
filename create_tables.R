library(dplyr)
library(tibble)
library(stringr)
library(tidyr)

###### create tables of poverty, neighborhoods ##########
poverty <- read.csv("poverty_la.csv")
## fpl
poverty_new<- poverty[1,]
poverty_new <- poverty_new %>% select(-contains(c('margin.of.error', 'Percent')))
poverty_table <- as.data.frame(t(poverty_new))
colnames(poverty_table) <- poverty_table[1,]
poverty_table <- tibble::rownames_to_column(poverty_table, var = "census")
poverty_table<- poverty_table[-1,]


tracts <- vector(mode = "character", length = nrow(poverty_table)) 
for (i in 1:nrow(poverty_table)) {
  name <- poverty_table$census[i]
  tracts[i] <- paste(unlist(str_extract_all(name, "\\d+")), collapse = "")
}
tracts <- paste0(tracts, strrep("0", 6-nchar(tracts)))
poverty_table$tracts <- tracts

poverty_table <- poverty_table %>% relocate(`Population for whom poverty status is determined`, .after = last_col())

poverty_table$census <- ifelse(grepl("Total", poverty_table$census), "total", poverty_table$census)
poverty_table$census <- ifelse(grepl("Below.poverty", poverty_table$census), "below_pov", poverty_table$census)

poverty_table_clean <- poverty_table %>%
  pivot_wider(names_from = census, values_from = `Population for whom poverty status is determined`)

## 200fpl
poverty_new200<- poverty[49,]
poverty_new200 <- poverty_new200 %>% select(-contains(c('margin.of.error', 'Percent')))
poverty_table200 <- as.data.frame(t(poverty_new200))
colnames(poverty_table200) <- poverty_table200[1,]
poverty_table200 <- tibble::rownames_to_column(poverty_table200, var = "census")
poverty_table200<- poverty_table200[-1,]


tracts <- vector(mode = "character", length = nrow(poverty_table200)) 
for (i in 1:nrow(poverty_table200)) {
  name <- poverty_table200$census[i]
  tracts[i] <- paste(unlist(str_extract_all(name, "\\d+")), collapse = "")
}
tracts <- paste0(tracts, strrep("0", 6-nchar(tracts)))
poverty_table200$tracts <- tracts

colnames(poverty_table200)[2] <- "pov_200"

poverty_table200 <- poverty_table200 %>% relocate(`pov_200`, .after = last_col())


poverty_table200$census <- ifelse(grepl("Total", poverty_table200$census), "below_pov200", poverty_table200$census)
poverty_table200$census <- ifelse(grepl("Below.poverty", poverty_table200$census), "na", poverty_table200$census)

poverty_table200_clean <- poverty_table200 %>%
  pivot_wider(names_from = census, values_from = `pov_200`)

poverty_table200_clean <- poverty_table200_clean[, -3]

#### combined table #####
poverty_la <- left_join(poverty_table_clean, poverty_table200_clean, by = 'tracts')

names(poverty_la)[names(poverty_la) == "tracts"] <- "tract"

#write.csv(poverty_la, "la_poverty_table.csv", row.names = FALSE)


