library(tidyverse)
# DATA SOURCE: https://aqs.epa.gov/aqsweb/airdata/download_files.html

ozone <- read_csv("data/hourly_44201_2014.csv",
  col_types = cols(
    .default = col_character(),
    `Parameter Code` = col_double(),
    POC = col_double(),
    Latitude = col_double(),
    Longitude = col_double(),
    `Date Local` = col_date(format = ""),
    `Time Local` = col_time(format = ""),
    `Date GMT` = col_date(format = ""),
    `Time GMT` = col_time(format = ""),
    `Sample Measurement` = col_double(),
    MDL = col_double(),
    Uncertainty = col_character(),
    Qualifier = col_character(),
    `Date of Last Change` = col_date(format = "")
    )
)
unique(ozone$Qualifier)
unique(ozone$Uncertainty)
str(ozone)
unique(ozone$`State Name`)
names(ozone) <- make.names(names(ozone))
str(ozone)
names(ozone)
unique(ozone$State.Code)

# Read file showing counties that are on the Atlantic, Pacific, or Gulf coasts
coastal.counties <- read_csv("data/CountyCoastAssign.csv", col_types = "cccc")
head(coastal.counties)
tail(coastal.counties)
tail(ozone)
head(ozone)
View(ozone)

# Do not include Puerto Rico or Mexico
statecodes <- c("01","02","04","05","06","08","09","10","11","12","13","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","44","45","46","47","48","49","50","51","53","54","55","56")

"06" %in% statecodes
usozone <- filter(ozone, ozone$State.Code %in% statecodes )
nrow(usozone)
nrow(ozone)

length(unique(usozone$State.Code)) == 51

unique(usozone$Parameter.Name)
unique(usozone$Parameter.Code)

# Ok, we do not need that one

unique(usozone$Site.Num)

working <- transmute(usozone, fipscode = paste0("#",State.Code,County.Code), Latitude, Longitude, Sample.Measurement, State.Name)
head(working)
coastaltagged <- merge(working,coastal.counties, by.x = "fipscode", by.y = "county") %>%
  filter(! is.na(Sample.Measurement))

unique(coastaltagged$g01005name)
names(coastaltagged)
names(coastaltagged) <- c("fipscode","Latitude","Longitude","OzonePPM","State","County","groupkey","groupname")


coastaltagged1 <- mutate(coastaltagged, coastal = 
                          case_when( groupname == "Gulf" | groupname == "Atlantic" | groupname == "Pacific" ~ "seacoast",
                                     TRUE ~ "land"))
                                     
unique(coastaltagged1$groupname)

coastaltagged1$groupname[is.na(coastaltagged1$groupname)] <- "Inland"

table(coastaltagged1$groupname)

summary(coastaltagged1$OzonePPM)
# looking good!

quantile(coastaltagged1$OzonePPM, seq(0,1, 0.1))

#from book:
par(las = 2, mar = c(10, 4, 2, 2), cex.axis = 0.8)
boxplot(OzonePPM ~ coastal, coastaltagged1, range = 0, ylab = "Ozone level (ppm)")
boxplot(OzonePPM ~ groupname, coastaltagged1, range = 0, ylab = "Ozone level (ppm)")
boxplot(OzonePPM ~ State, coastaltagged1, range = 0, ylab = "Ozone level (ppm)")
unique(coastaltagged1$groupname)

coastaltagged1$region <- factor(ifelse(coastaltagged1$Longitude < -100,"West","East"))

group_by(coastaltagged1, region) %>%
          summarize(mean = mean(OzonePPM, na.rm = TRUE),
           median = median(OzonePPM, na.rm = TRUE))

# challenge the interpretation

filter(coastaltagged1, groupname == "Inland") %>%
  group_by(region) %>%
  summarize(mean = mean(OzonePPM, na.rm = TRUE),
            median = median(OzonePPM, na.rm = TRUE))

filter(coastaltagged1, coastal == "seacoast") %>%
  group_by(region) %>%
  summarize(mean = mean(OzonePPM, na.rm = TRUE),
            median = median(OzonePPM, na.rm = TRUE))
