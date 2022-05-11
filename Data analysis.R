library(tidyverse)
library(lubridate)

# Create running total
library(tibbletime)

# For plotting in dual axis
library(hrbrthemes)
library(patchwork)

# PART 1: FIRST JAPANESE CHERRY BLOOM DATA

bloom_file = "sakura_first_bloom_dates.csv"

first_bloom <- read_csv(bloom_file)

summary(first_bloom)

# The data is in the wide format and must be converted to long format.  

first_bloom <- gather(first_bloom, key = Year, value = Date, `1953`:`2020`)

# Confirm that data is appropriately transformed to long format

View(first_bloom)

# Convert columns to appropriate type and rename columns

first_bloom <- first_bloom %>% 
  mutate(Date = mdy(Date),
         `Site Name` = as_factor(`Site Name`),
         Notes = as_factor(Notes), 
         Year = as.integer(Year)) %>% 
  rename(c(Site = `Site Name`, 
           Currently_Being_Observed = `Currently Being Observed`))

# Determine the quantity of missing Date

summary(first_bloom)

# According to the summary data for the Date column there are 977 missing dates 
# (15%), which are irretrievable, since it is likely that it was never collected.  
# These observations are removed from the analysis.  For the notes on the genus 
# 84.4% are missing.

First_Bloom_Date_is_na <- first_bloom %>% 
  filter(is.na(Date))

summary(First_Bloom_Date_is_na) 

unique(First_Bloom_Date_is_na$Site)
unique(first_bloom$Site)

# 57 of the 102 cities have missing date data.
# Understand the distribution of missing values for different sites.  

count_and_prop_na_each_site <- first_bloom %>% 
  group_by(Site) %>% 
  summarise(count = sum(is.na(Date)),
            prop_na = round(mean(is.na(Date))*100)) %>% 
            arrange(desc(prop_na))

View(count_and_prop_na_each_site)  

# Remove data without dates for first bloom (i.e. 977 rows of 6926 rows), 
# Since these data are irretrievable.

no_na_first_bloom <- 
  first_bloom %>% drop_na(Date)

# Calculate days of the year for first bloom

days_of_the_year <- no_na_first_bloom %>% 
  mutate(Days_of_Year = yday(Date)
  ) 

# Determine the number of unique cherry tree genus.

unique(no_na_first_bloom$Notes)

# Since the notes for genus is verbose, I initialized the notes..

days_of_the_year <- days_of_the_year %>% 
  mutate(Notes = fct_recode(Notes,
                            "KIC" = "Kurile Island Cherry (Cerasus nipponica var. kurilensis)", 
                            "SC" = "Sargent cherry (Prunus sargentii)",                                    
                            "TC" = "Taiwan cherry (Prunus campanulata)",                                       
                            "SC_TC" = "Until 1994 Sargent Cherry, from 1995 to 2006 they were Yoshino Cherry."
  ))

# With the large number of sites in the data, it is best to use facet_wrap
# by cherry species for each site.  

days_of_the_year %>%  
  ggplot(aes(x = Year, y = Days_of_Year)) +
  geom_point() + 
  geom_smooth() +
  facet_wrap(~ Notes)  # The TC genus has unusual bloom day, while the other 
# four categories have similar time frames.  The NA notes category is more 
# scattered.  

# Determine the distribution of the bloom day for the different genus including NA notes.  

days_of_the_year %>%  
  ggplot(aes(x = Days_of_Year, color = Notes)) +
  geom_freqpoly(size = 1) +
  coord_cartesian(ylim = c(0, 200)) +
  theme(legend.text = element_text(colour="blue", size=10, 
                                   face="bold"))

# The distribution are very similar for SC, SC_TC, KIC.  NA notes could be a 
# combination # SC, SC_TC, KIC and other varieties that bloom earlier 
# (70-120 days).

# The "TC" = "Taiwan cherry (Prunus campanulata)" have very early bloom day
# and very late bloom date. Determine if location is the determining factor.  

TC <- days_of_the_year %>%  
  filter(Notes == 'TC' )

# Find sites that TC sites

unique(TC$Site)

# It's found in these nine different site: 'Naze', 'Ishigaki Island', 'Iriomote Island', 
# 'Kumejima','Minamidaitojima', 'Miyakojima', 'Nago', 'Naha', 'Yonaguni Island'

TC_Site <- c('Naze', 'Ishigaki Island', 'Iriomote Island', 'Kumejima',
             'Minamidaitojima', 'Miyakojima', 'Nago', 'Naha', 'Yonaguni Island')

TC %>% 
  ggplot() +
  geom_freqpoly(aes(Days_of_Year)) +
  coord_cartesian(ylim = c(0,20))

# Combine the bimodal characteristics of TC by adding 365 days to the bloom day less 
# than 60.

days_of_the_year %>% 
  filter(Days_of_Year < 60 | Days_of_Year > 300) %>% 
  mutate(Days_of_Year = case_when(
    Days_of_Year < 60 ~ Days_of_Year + 365)) %>% 
  ggplot() +
  geom_freqpoly(aes(Days_of_Year))

# Find the amount of notes is NA data and determine its characteristics
# (i.e. sites, days of the year)

notes_na <- days_of_the_year %>%  
  filter(is.na(Notes)) # 4899 of 5959 are NA.  It would be unwise to remove a 
# significant amount of data. These NA observations are analyzed separately  

unique(notes_na$Site) # 81 of 102 site have notes that is NA.

intersect(notes_na$Site, TC$Site) # As demonstrated by the previous frequency
# plot, no sites in NA are in TC sites.

# Analysis of SC, SC_TC, KIC genus

not_TC_or_na <- days_of_the_year %>%  
  filter(Notes %in% c('SC', 'SC_TC', 'KIC'))

intersect(notes_na$Site, not_TC_or_na$Site) # SC, SC_TC and KIC sites don't
# overlap NA sites

not_TC_or_na %>% 
  ggplot(aes(x = Year, y = Days_of_Year)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ Notes)

# SC, KIC, and SC_TC show similar trends. Correlate these

days_of_the_year %>%  
  filter(is.na(Notes)) %>% 
  ggplot(aes(x = Year, y = Days_of_Year)) +
  geom_point() +
  geom_smooth(aes(group = Site), se = FALSE) +
  theme(axis.text.x = element_text(angle = 60, vjust=0.5),
        legend.position="none") #All the smooth fit show very similar trends. 
# Can temperature explain the smooth fit characteristics.  

# PART 2: TEMPERATURE DATA

# Since cherry blossom is affected by the number of freezing days, I will count
# the number of sub-zero temperature from the end of previous year with the 
# current year before first blossom date.  
# Left join to combine Site temperature with first bloom date

Site_temperature <- read_csv("Japanese_City_Temps.csv")

Site_temperature <- Site_temperature %>%
  pivot_longer(!Date, names_to = 'Site', values_to = 'Temperature') %>% 
  mutate(Date = mdy(Date))

# PART 3: JOIN FIRST BLOOM AND TEMPERATURE DATA

join_temperature_first_bloom <- Site_temperature %>%
  left_join(days_of_the_year, by = c('Site', 'Date'))

# Count the number of freezing days before first bloom. Use rollify function
# with a window of 240 days. 

rolling_count_240 <- rollify(sum, window = 240)

Count_Freezing_Bloom_Day <- join_temperature_first_bloom %>% 
  group_by(Site) %>% 
  mutate(below_freezing = Temperature < 0,
         Num_of_Freezing_Days = rolling_count_240(below_freezing))

# Count_Freezing_Bloom_Day <- Count_Freezing_Bloom_Day %>% 
#   filter(!is.na(Year))

View(Count_Freezing_Bloom_Day)

Count_Freezing_Bloom_Day %>% 
  ggplot(aes(x = Year, y = Num_of_Freezing_Days)) +
  geom_point() +
  geom_smooth() +
  facet_wrap('Notes')

Count_Freezing_Bloom_Day %>% 
  filter(Notes %in% c('SC', 'KIC', 'SC_TC')) %>% 
  ggplot(aes(x = Year, y = Num_of_Freezing_Days)) +
  geom_point() +
  geom_smooth() +
  facet_wrap('Notes')

coeff <- 1

# A few constants
Days_of_Year_Color <- "red"
Num_of_Freezing_Days_Color <- "black"

ggplot(Count_Freezing_Bloom_Day, aes(x=Year)) +
  
  geom_smooth( aes(y=Days_of_Year), size=1, color=Days_of_Year_Color) + 
  geom_smooth( aes(y=(Num_of_Freezing_Days / coeff)), 
               size=1, color=Num_of_Freezing_Days_Color) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Days of Year",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Number of Freezing Days")
  ) + 
  facet_wrap('Notes') +
  theme_ipsum()

TC_counts <- Count_Freezing_Bloom_Day %>% 
  filter(Notes == 'TC')

View(TC_counts)

# There seems to be a correlation between earlier bloom dates with decreasing
# number of freezing days.  
