library("sf")
library("raster")
library("prism")
library("terra")
library("dplyr")
library("data.table")
library("future")
library(sp)
library("ggplot2")
library(rgdal)


########## LA census tracts and poverty data #############
#poverty data
la_neighborhoods <- read.csv("Below_Poverty_tract_7959652566936946184.csv")
la_neighborhoods$tract <- as.character(la_neighborhoods$tract)
la_neighborhoods$tract <- substr(la_neighborhoods$tract, 5, nchar(la_neighborhoods$tract))

la_neighborhoods <- la_neighborhoods[c("tract", "csa")]
poverty_tracts <- read.csv("la_poverty_table.csv")
poverty_tracts$tract <- as.character(poverty_tracts$tract)

la_poverty <- left_join(la_neighborhoods, poverty_tracts, by = "tract")


#shapefiles of LA census tracts
la_sf <- st_read("2020_Census_Tracts/2020_Census_Tracts.shp")
colnames(la_sf)[colnames(la_sf) == "CT20"] <- "tract"

tracts_neighbor <- right_join(la_sf, la_poverty, by = "tract")
tracts_neighbor$total <- as.numeric(gsub(",", "", tracts_neighbor$total))
tracts_neighbor$below_pov <- as.numeric(gsub(",", "", tracts_neighbor$below_pov))
tracts_neighbor$below_pov200 <- as.numeric(gsub(",", "", tracts_neighbor$below_pov200))

neighborhood_sf <- tracts_neighbor %>%
  group_by(csa) %>%
  summarize(tot_pop = sum(total,na.rm = TRUE), below_pov = sum(below_pov, na.rm = TRUE), below_pov200 = sum(below_pov200,na.rm = TRUE))

neighborhood_sf$pct_below_fpl <- neighborhood_sf$below_pov/neighborhood_sf$tot_pop
neighborhood_sf$pct_below200_fpl <- neighborhood_sf$below_pov200/neighborhood_sf$tot_pop

neighborhood_sf$pov_lev <- ifelse(neighborhood_sf$pct_below_fpl > .40, "extreme", ifelse(neighborhood_sf$pct_below_fpl > .20, "high", "low"))
neighborhood_sf$pov_lev_200 <- ifelse(neighborhood_sf$pct_below200_fpl > .40, "extreme", ifelse(neighborhood_sf$pct_below200_fpl > .20, "high", "low"))


plot(st_geometry(neighborhood_sf)) #plots just the geometry



#map of poverty levels in LA
ggplot() +
  geom_sf(data = neighborhood_sf, aes(fill = pct_below200_fpl), size = .1) +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())+
  scale_fill_continuous(name = "Poverty", limits = c(0.0, 1.0), breaks = seq(0, 1, .1), low = "lightblue", high = "darkred",
                        guide = "colorbar") +
  ggtitle("Below 200 FPL")

ggplot() +
  geom_sf(data = neighborhood_sf, aes(fill = pov_lev_200)) +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())+
  scale_fill_manual(values = c("darkred", "goldenrod1", "lightblue"),
                    name = "Poverty Level",
                    labels = c("Extreme (>40%)", "High (>20%)", "Low (<20%)"))+
  ggtitle("Below 200 FPL")

ggplot() +
  geom_sf(data = neighborhood_sf, aes(fill = pct_below_fpl), size = .1) +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())+
  scale_fill_continuous(name = "Poverty", limits = c(0.0, 1.0), breaks = seq(0, 1, .1), low = "lightblue", high = "darkred",
                        guide = "colorbar")

ggplot() +
  geom_sf(data = neighborhood_sf, aes(fill = pov_lev)) +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())+
  scale_fill_manual(values = c("darkred", "gold", "lightblue"),
                    name = "Poverty Level",
                    labels = c("Extreme (>40%)", "High (>20%)", "Low (<20%)"))
#darkred, gold, mediumseagreen

############# Temperature Data #############
#get temperature data
prism_set_dl_dir("prism_tmax/")
file_names <- prism_archive_ls()

########## Get tables of mean tmax ###########
mean_tmax_df <- data.frame()
poverty_sv <- vect(neighborhood_sf)
poverty_sf <- 
  st_as_sf(poverty_sv) %>%
  mutate(ID := seq_len(nrow(.)))

#id, mean_tmax, year, month
for (file in file_names) {
  column_name <- rlang::sym(file)
  tmax_sr <- rast(paste0("prism_tmax/", file, "/", file, ".bil"))
  tmax_values <- extract(tmax_sr, poverty_sv)
  
  Year <-  as.integer(substr(file, 25, 28))
  Month <-  as.integer(substr(file, 29, 30))
  
  mean_tmax <- tmax_values %>%
    group_by(ID) %>%
    summarize(tmax = mean(!!column_name)) %>%
    mutate(year = Year, month = Month)
  
  pov_temp <-
    left_join(poverty_sf, mean_tmax, by = "ID") %>%
    st_drop_geometry()
  pov_temp <- pov_temp[,c("csa", "tmax", "pov_lev_200", "year", "month")]
  
  mean_tmax_df <- bind_rows(mean_tmax_df, pov_temp)
}
#write.csv(mean_tmax_df, "mean_tmax_neighborhood.csv")

#### Make plots of temperature data ####
#average summer temperatures per year by poverty level
mean_tmax <- read.csv("mean_tmax_neighborhood.csv", row.names = NULL)
mean_tmax <- subset(mean_tmax, select = -X)

#.2 threshold
mean_tmax$pov_lev200_2 <- ifelse(mean_tmax$pov_lev_200 %in% c("high", "extreme"), "high", mean_tmax$pov_lev_200)

mean_tmax$tmax_F <- (9/5)*mean_tmax$tmax +32

pov_groups_2 <- mean_tmax %>% 
  group_by(year, pov_lev200_2) %>%
  summarize(tmax = mean(tmax_F, na.rm = TRUE))

pov_groups_2 <- pov_groups_2[complete.cases(pov_groups_2$pov_lev200_2), ]

ggplot(pov_groups_2, aes(x = year, y = tmax))+
  geom_point(aes(color = pov_lev200_2), size = 2) +
  geom_line(aes(color = pov_lev200_2), size = 2, alpha = .2) +
  labs(title = "Average summer temperature by LA Neighborhood",
       subtitle = "Threshold poverty level = 0.2",
       y = "Average summer temperature",
       color = "Poverty Level") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"),
        axis.title.x = element_blank(),
        legend.title = element_text(face = "bold"))

####### map by temperature #########
#2023 data
mean_tmax_2023 <- mean_tmax[mean_tmax$year == 2023, ]
mean_tmax_2023<- mean_tmax_2023 %>%
  group_by(csa) %>%
  summarize(mean_tmax = mean(tmax_F, na.rm = TRUE))
pov_tmax_2023 <- left_join(neighborhood_sf, mean_tmax_2023, by = "csa")

ggplot() +
  geom_sf(data = pov_tmax_2023, aes(fill = mean_tmax), size = .1) +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())+
  scale_fill_continuous(name = "Temperature", limits = c(65, 95), breaks = seq(65, 95, 5), low = "gold", high = "darkred",
                        guide = "colorbar") +
  ggtitle("Max Temperature in 2023")

########## plot of income and temperature #########
median_income <- read.csv("la_median_income.csv")
median_income$tract <- substr(median_income$tract, 5, nchar(median_income$tract))
med_income <- median_income[c("tract", "med_hh_income")]
mean_tmax_tract <- mean_tmax %>% 
  group_by(tract) %>%
  summarize(tmax = mean(tmax_F, na.rm = TRUE))
income_tmax <- left_join(mean_tmax_tract, med_income, by = "tract")
plot(income_tmax$med_hh_income, income_tmax$tmax)
lm(income_tmax$tmax~income_tmax$med_hh_income)
summary(lm(income_tmax$tmax~income_tmax$med_hh_income))


