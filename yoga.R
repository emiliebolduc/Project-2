# Read CSV from URL 
yogaURL <- "https://data.world/dotslashmaggie/google-trends-yoga/workspace/file?filename=20160502_YogaByStateMonth.csv"
yoga <- read.csv("https://query.data.world/s/64O-zcKa02sO0C7fZDCjSyZlDJmZ7e", header=TRUE, stringsAsFactors=FALSE)
head(yoga)

# Remove 1st row of data
yoga <- yoga[-c(1:1), ]
head(yoga)

# Write .csv file
write.csv(yoga, file = "yoga.csv", row.names = FALSE)

# Tidy data with tidyr and dplyr 
library(dplyr)
yoga <- dplyr::tbl_df(yoga)
yoga

# I wanted to rename all the state names to be cleaner using regular expressions, as I thought it would be faster and easier than typing in all the code (like below). But, unfortunately, I struggled to figure out how. 
# then I tried to rename it using dplyr::rename and %>% but that didn't work either
yoga <- rename(yoga, Alabama = Alabama..us.al., Alaska = Alaska..us.ak., Arizona = Arizona..us.az., Arkansas = Arkansas..us.ar., California = California..us.ca., Colorado = Colorado..us.co., Connecticut = Connecticut..us.ct., Delaware = Delaware..us.de., DC = District.of.Columbia..us.dc., Florida = Florida..us.fl., Georgia = Georgia..us.ga., Hawaii = Hawaii..us.hi., Idaho = Idaho..us.id., Illinois = Illinois..us.il., Indiana = Indiana..us.in., Iowa = Iowa..us.ia., Kansas = Kansas..us.ks., Kentucky = Kentucky..us.ky., Louisiana = Louisiana..us.la., Maine = Maine..us.me., Maryland = Maryland..us.md., Massachusetts = Massachusetts..us.ma., Michigan = Michigan..us.mi., Minnesota = Minnesota..us.mn., Mississippi = Mississippi..us.ms., Missouri = Missouri..us.mo., Montana = Montana..us.mt., Nebraska = Nebraska..us.ne., Nevada = Nevada..us.nv., New_Hampshire = New.Hampshire..us.nh., New_Jersey = New.Jersey..us.nj., New_Mexico = New.Mexico..us.nm., New_York = New.York..us.ny., North_Carolina = North.Carolina..us.nc., North_Dakota = North.Dakota..us.nd., Ohio = Ohio..us.oh., Oklahoma = Oklahoma..us.ok., Oregon = Oregon..us.or., Pennsylvania = Pennsylvania..us.pa., Rhode_Island = Rhode.Island..us.ri., South_Carolina = South.Carolina..us.sc., South_Dakota = South.Dakota..us.sd., Tennessee = Tennessee..us.tn., Texas = Texas..us.tx., Utah = Utah..us.ut., Vermont = Vermont..us.vt., Virginia = Virginia..us.va., Washington = Washington..us.wa., West_Virginia = West.Virginia..us.wv., Wisconsin = Wisconsin..us.wi., Wyoming = Wyoming..us.wy.)
yoga 

# Separate "Year-Month" into two columns
yoga <- tidyr::separate(yoga, X, c("Year", "Month"))
yoga
ncol(yoga)

# make the States observations from variables with tidyr 'gather' function
library(tidyr)
YogaLong <- gather(yoga, "State", "n", 3:53)
YogaLong

# Rename "n" to "Searches"
YogaLong <- dplyr::rename(YogaLong, Searches = 'n')
YogaLong

# Change "Year" and "Month" columns from characters to numeric?
YogaLong$Year <- as.numeric(as.character(YogaLong$Year))
YogaLong$Month <- as.numeric(as.character(YogaLong$Month))
YogaLong

# make a pretty table
library(DT)
datatable(YogaLong)

#Analysis
library(ggplot2)

# when was there the most interest in yoga and where??
arrange(YogaLong, desc(Searches))

# when was there the least interest in yoga and where?
arrange(YogaLong, Searches)

#group by state and year and see mean, min, and max of searches by each
#which state has the highest interest in yoga based on their mean number of google searches from 2004-2016?
State <- YogaLong %>% 
  group_by(State) %>% 
  summarise(Mean = mean(Searches), Min = min(Searches), Max = max(Searches), n = n())
  
State <- arrange(State, desc(Mean))

State
ggplot(State, aes(x=State, y=Mean, fill=Mean)) +
  geom_bar(stat = "identity") +
  xlab("State") + ylab("Mean of the Indexed Google Searches by Month") +
  ggtitle("Interest in Yoga Based on Google Searches by State from 2004-2016") +
  theme(plot.title = element_text(lineheight = .8, face = "bold")) +
  theme(axis.text.x = element_text(angle = 60, vjust = .5, size = 9)) +
  theme_classic() +
  coord_flip()

# what is the interest in yoga year over year across the entire US?
Year <- YogaLong %>% 
  group_by(Year) %>% 
  summarise(Mean = mean(Searches), Min = min(Searches), Max = max(Searches), n = n())
Year

ggplot(Year, aes(x=Year, y=Mean)) +
  geom_line(size = .5, colour = "blue") +
  geom_point(size = 1, colour = "light blue") +
  ggtitle("Interest of Yoga Based on the Number of Google Searches by Year from 2004-2016") +
  ylab("Mean of the Indexed Google Searches") +
  theme_classic() 

# when during the year do people search most for yoga
Month <- YogaLong %>% 
  group_by(Month) %>% 
  summarise(Mean = mean(Searches), Min = min(Searches), Max = max(Searches), n = n())
Month

library(lubridate)
ggplot(Month, aes(x=month(Month, label = TRUE, abbr = TRUE), y=Mean, group=1)) +
  geom_line(size = .5, colour = "navy") +
  geom_point(size = 1, colour = "lime green") +
  ggtitle("Interest of Yoga Based on the Number of Google Searches by Month") +
  ylab("Indexed Google Searches") + xlab("Month") +
  theme_classic() 
#source: https://stackoverflow.com/questions/42425195/ggplot-data-year-over-year-axis-labels-not-showing
#Conclusion