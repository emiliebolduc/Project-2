# Read CSV from URL 
theURL <- "https://raw.githubusercontent.com/fivethirtyeight/data/master/marriage/both_sexes.csv"
marriage <- read.csv(theURL, header = TRUE, sep = ",")
head(marriage)

# Write CSV file
write.csv(marriage, file = "marriage.csv", row.names = FALSE)

# tidy the data
library(dplyr)
library(tidyr)

#subset data to include marriage rate data for just 25-34 year-old demographics, excluding kids:
marriage2534 <- marriage[c(2,4:21)]
head(marriage2534)
colnames(marriage2534)

# change column names to more readable
colnames(marriage2534)<- c("Year", "All", "High School Graduate", "Some College", "Bachelor's Degree", "Bachelor's and Some Graduate", "Graduate Degree", "White", "Black", "Hispanic", "New England", "Mid-Atlantic", "Midwest", "South", "Mountain West", "Pacific", "Low-Income", "Middle-Income", "Upper-Income")
colnames(marriage2534)
head(marriage2534)
ncol(marriage2534)

# make observations from variables with tidyr 'gather' function
marriage2534 <- gather(marriage2534, "Demographics", "n", 2:19)
head(marriage2534, 30)

# Rename "n" to "Single Rate," because according to the data source, the figures in "n" represent the share of the relevant population that has never been married. Adding a column with figures that are the inverse of that, and naming it "Marriage Rate."  
marriage_tidy <- dplyr::rename(marriage_tidy, Single_Rate = n)

marriage_tidy <- marriage2534 %>% 
  mutate(Marriage_Rate = 1 - n)

head(marriage_tidy, 40)

# change Single_Rate and Marriage_Rate numeric
marriage_tidy <- transform(marriage_tidy, Single_Rate = as.numeric(Single_Rate))
marriage_tidy <- transform(marriage_tidy, Marriage_Rate = as.numeric(Marriage_Rate))
which(is.na(marriage_tidy))
marriage_tidy

# round Single_Rate and Marriage_Rate to the hundredth decimal place
marriage_tidy[, "Marriage_Rate" ] = format(round(marriage_tidy[, "Marriage_Rate" ], 2), nsmall = 2)
marriage_tidy[, "Single_Rate" ] = format(round(marriage_tidy[, "Single_Rate" ], 2), nsmall = 2)

# convert to a local data frame
marriage_tidy <- tbl_df(marriage_tidy)
marriage_tidy


# Make a pretty data table
library(DT)
datatable(marriage_tidy)
# reference: http://rpubs.com/jillenergy/313578

#annalysis - interested in exploring the declining marriage rates for millenials by All, Race, Education, Income Level, and finally Region (US)
#All
All <- filter(marriage_tidy, Demographics == "All", Year >= 2000)
All
ggplot(All, aes(Year, Marriage_Rate, group=1)) +
  geom_line() +
  geom_point() +
  expand_limits(y=.5) +
  scale_x_continuous(limits = c(2000, 2013)) +
  theme_linedraw() +
  ggtitle("Declining Marriage Rates in All People Ages 25-34") +
  ylab("Marriage Rate") +
  theme(plot.title = element_text(lineheight = .8, face = "bold"))

#Race
Race <- filter(marriage_tidy, Demographics == "White" | Demographics == "Black" | Demographics == "Hispanic", Year >= 2000)
Race
ggplot(Race, aes(x = Year, y = Marriage_Rate, group = Demographics, colour = Demographics)) +
  geom_line() +
  geom_point() +
  scale_y_continuous() +
  scale_x_continuous(limits = c(2000, 2013)) +
  theme_linedraw() +
  ggtitle("Declining Marriage Rates by Race in People Ages 25-34") +
  ylab("Marriage Rate") +
  theme(plot.title = element_text(lineheight = .8, face = "bold"))

#Education
Education <- filter(marriage_tidy, Demographics == "Graduate Degree" | Demographics == "Bachelor's and Some Graduate" | Demographics == "Bachelor's Degree" | Demographics == "Some College" | Demographics == "High School Graduate", Year >= 2000)
Education
ggplot(Education, aes(x = Year, y = Marriage_Rate, group = Demographics, colour = Demographics)) +
  geom_line() +
  geom_point() +
  scale_y_continuous() +
  scale_x_continuous(limits = c(2000, 2013)) + 
  ggtitle("Declining Marriage Rates by Education Level in People Ages 25-34") +
  theme_classic() +
  ylab("Marriage Rate") +
  theme(plot.title = element_text(lineheight = .8, face = "bold"))

#Income Level
Income <- filter(marriage_tidy, Demographics == "Low-Income" | Demographics == "Middle-Income" | Demographics == "Upper-Income", Year >= 2000)
Income
ggplot(Income, aes(x = Year, y = Marriage_Rate, group = Demographics, colour = Demographics)) +
  geom_line() +
  geom_point() +
  scale_y_continuous() +
  scale_x_continuous(limits = c(2000, 2013)) + 
  ggtitle("Declining Marriage Rates by Income Level in People Ages 25-34") +
  theme_classic() +
  ylab("Marriage Rate") +
  theme(plot.title = element_text(lineheight = .8, face = "bold"))

#Region US
Region <- filter(marriage_tidy, Demographics == "New England" | Demographics == "Mid-Atlantic" | Demographics == "Midwest" | Demographics == "South" | Demographics == "Mountain West" | Demographics == "Pacific", Year >= 2000)
Region
ggplot(Region, aes(x = Year, y = Marriage_Rate, group = Demographics, colour = Demographics)) +
  geom_line() +
  geom_point() +
  scale_y_continuous() +
  scale_x_continuous(limits = c(2000, 2013)) + 
  ggtitle("Declining Marriage Rates by Income Level in People Ages 25-34") +
  theme_classic() +
  ylab("Marriage Rate") +
  theme(plot.title = element_text(lineheight = .8, face = "bold"))

