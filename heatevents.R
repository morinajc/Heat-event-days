# Plotting and summarizing VA and USA heat days
library(ggplot)
library(reshape2)
library(dplyr)
library(patchwork)
library(lubridate)
library(ggthemes)
library(ggpubr)

# 1. VA Heat events by weather station 1970 - 2022 ####
# Read in data and format data, add year column, select only May - Sept
va_50 <-read.csv("VA_50.csv")
va_50$Date <- as.Date(va_50$Date , format = "%m/%d/%y") # sometimes it is %Y-%m-%d
va_50[,"Year"]<-format(va_50[,"Date"],"%Y")
summer <- va_50 %>% 
  filter(!lubridate::month(as.Date(Date)) %in% c(10,11, 12, 1, 2, 3, 4))

# Convert df to long format and count events
summer_long <- melt(summer, id.vars = c( "Date", "Year"), 
                    variable.name = "Station")

summer_count <- summer_long %>% 
  filter(!is.na(value)) %>%
  group_by(Station, Year) %>% 
  summarise( 
    n = n(),
    count = sum(value))

# Subset data, only include years where there was at least 100 days available, and exclude select dats for two stations missing data
summer_count_100 <- subset(summer_count, n > 100 )
summer_clean <-summer_count_100[!(summer_count_100$Station == "CHO" & summer_count_100$Year < 1999), ]         
summer_clean2 <-summer_clean[!(summer_clean$Station == "NYG" & summer_clean$Year < 1985), ]         

# Convert year to numeric
summer_clean2$Year <-as.numeric(summer_clean2$Year)

# Plot by station
VA_50_plot <- ggplot(summer_clean2, aes(x=Year, y= count, group=1 ))+
  geom_line()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust=1,color="black"),
        axis.text.y = element_text(color="black"),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")+
  facet_wrap(.~Station, ncol=3)+
  labs(y = "Heat Event Days", x=" ")+
  scale_y_continuous(expand = c(0,0),limits = c(0,120,30))+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")+
  stat_regline_equation()

# Plot all counts by year
combo <- ggplot(summer_count_100_e, aes(x= Year, y = count))+
  geom_smooth()+
  geom_point(shape = 21, fill = "white", color = "black", size=2)+
  scale_y_continuous(expand = c(0,0),limits = c(0,120,30))+
  theme_fivethirtyeight()+
  scale_x_continuous(breaks=c(1970, 1980, 1990, 2000, 2010,2020))+
  labs(x=" ",
       y="Annual Heat Events (days)",
       title="Summer heat events increased significantly after 2000",
       subtitle="Humid and dry tropical days in Virginia since 1970",
       caption="Data from Spatial Synoptic Classification v3.0")

# 2. VA heat events 2016 - 2020 ####
# Read in VA heat data by station
VA <- read.csv("VA_days.csv",header=T)

# Generate summary for whole state across all 5 years
VA_avg <- VA %>% 
  group_by(Year) %>% 
  summarise( # creates columns in the new dataframe for each summary stat
    mean = mean(n),
    median = median(n),
    min = min(n),
    max = max(n),
    sd = sd(n),
    n1 = n(),
    se = sd/sqrt(n1))

# Graph events by station by year 
VA_station_plot <- ggplot(VA, aes(x=Year, y= n ))+
  geom_bar(fill = "darkred", color = "black", stat ="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust=1,color="black"),
        axis.text.y = element_text(color="black"),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")+
  facet_wrap(.~Station, ncol=3)+
  labs(y = "Heat Event Days", x=" ")+
  scale_y_continuous(expand = c(0,0),limits = c(0,120,30))

# Graph all of VA
# avg +/- sd
VA_total_plot <- ggplot(VA_avg,(aes(x = Year, y = mean)))+
  geom_bar(stat="identity", fill = "darkred", color="black", size=0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(size=10, color="black"),
        axis.text.y = element_text(size=10, color="black"),
        panel.grid.minor.x = element_blank())+
  scale_y_continuous(expand = c(0,0), limits = c(0,120,10))+ 
  geom_errorbar(aes(ymin = mean - sd,ymax = mean + sd),
                linewidth=0.4,
                position = "dodge", 
                width = 0.3)+
  labs(y = " Heat Event Days ", x=" ")
  
# boxplot
VA_total_boxplot <- ggplot(VA,(aes(x = Year, y = n, group=Year)))+
  geom_boxplot( fill = "darkred", color="black")+
  theme_bw()+
  theme(axis.text.x = element_text(size=10, color="black"),
        axis.text.y = element_text(size=10, color="black"),
        panel.grid.minor.x = element_blank())+
  scale_y_continuous(expand = c(0,0), limits = c(0,120,10))+ 
  labs(y = " Heat Event Days ", x=" ")
  
# combine into one figure
combo2 <- VA_station_plot | (VA_total_plot / VA_total_boxplot ) 

# add letter
combo_va <- combo2 + plot_annotation(tag_levels = 'A')

# Save figure
ggsave("VA Heat Events.png", plot = combo_va, device = "png", path = NULL, 
       scale = 1, width = 8.27, height = 6.54, units = "in", dpi = 800)

# 3. VA extreme heat events 2016 - 2020 #####
# Read in data
VAEX <- read.csv("extreme1.csv")

# Save date as date
VAEX$Date <- as.Date(VAEX$Date , format = "%m/%d/%y") # sometimes it is %Y-%m-%d

# Might need to drop column, use subset
VAEX = subset(VAEX, select = -c(Alt.Date))

# convert to long
VAEX_long <- reshape2::melt(VAEX, id.vars = c("Date"), 
                            variable.name = "Station")
# add year column
VAEX_long$year <- year(VAEX_long$Date)

# Summarize total days (excludes NA), and number of events
VAEX_count <- VAEX_long %>% 
  filter(!is.na(value)) %>%
  group_by(Station, year) %>% 
  summarise( 
    n = n(),
    count = sum(value))

write.csv(VAEX_count,"va extreme count.csv")

VAEX_avgs <- VAEX_count %>% 
  group_by(year) %>% 
  summarise( 
    n = n(),
    min = min(count),
    max = max(count),
    median = median(count),
    mean = mean(count),
    sd = sd(count))

# 4. USA heat events 2016 - 2020 ####
# Read in data
USA_heat <- read.csv("USA_HEAT.csv",header = T)

# save as date 
USA_heat$Date <- as.Date(USA_heat$Date , format = "%m/%d/%y") 

# Might need to drop column, use subset
USA_heat = subset(USA_heat, select = -c(Year))

# convert to long
USA_heat_long <- reshape2::melt(USA_heat, id.vars = c("Date"), 
                            variable.name = "Station")

# add year column
USA_heat_long$year <- year(USA_heat_long$Date)

# Summarize total days (excludes NA), and number of events
USA_count <- USA_heat_long %>% 
  filter(!is.na(value)) %>%
  group_by(Station, year) %>% 
  summarise( 
    n = n(),
    count = sum(value))

write.csv(USA_count,"usa count.csv")

USA_avgs <- USA_count %>% 
  group_by(year) %>% 
  summarise( 
    n = n(),
    min = min(count),
    max = max(count),
    median = median(count),
    mean = mean(count),
    sd = sd(count))

# plot station count whole US
USA_station_plot <- ggplot(USA_count, aes(x=year, y= count ))+
  geom_bar(fill = "darkorange", color = "black", stat ="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust=1,color="black"),
        axis.text.y = element_text(color="black"),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        legend.position = "none")+
  facet_wrap(.~Station, ncol=10)+
  labs(y = "Heat Event Days", x=" ")+
  scale_y_continuous(expand = c(0,0),limits = c(0,200,30))

ggsave("USA Heat Events.png", plot = USA_station_plot, device = "png", path = NULL, 
       scale = 1, width = 3500, height = 2500, units = "in", dpi = 800, limitsize=F)



USA_total_plot <- ggplot(USA_avgs,(aes(x = year, y = mean)))+
  geom_bar(stat="identity", fill = "darkorange", color="black", size=0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(size=10, color="black"),
        axis.text.y = element_text(size=10, color="black"),
        panel.grid.minor.x = element_blank())+
  scale_y_continuous(expand = c(0,0), limits = c(0,150,10))+ 
  geom_errorbar(aes(ymin = mean - sd,ymax = mean + sd),
                linewidth=0.4,
                position = "dodge", 
                width = 0.3)+
  labs(y = " Heat Event Days ", x=" ")

# boxplot - something's not right
USA_count$year <-as.character(USA_count$year)

USA_total_boxplot <- ggplot(USA_count,(aes(x = year, y = count, group=year)))+
  geom_boxplot( fill = "darkorange", color="black")+
  theme_bw()+
  theme(axis.text.x = element_text(size=10, color="black"),
        axis.text.y = element_text(size=10, color="black"),
        axis.title.y = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_y_continuous(limits = c(0,150,10),expand = c(0,1))+ 
  labs(y = " Heat Event Days ", x=" ")

# combine into one figure
combo_USA <- USA_total_plot | USA_total_boxplot
# add letter
combo_USA_heat <- combo_USA + plot_annotation(tag_levels = 'A')

ggsave("USA Heat Events.png", plot = combo_USA_heat, device = "png", path = NULL, 
       scale = 1, width = 6.28, height = 4.05, units = "in", dpi = 800)

# 5. USA extreme heat events 2016 - 2020 ####
# Read in data 
USA_EX <- read.csv("USAEX.csv", header=T)

# save as date 
USA_EX$Date <- as.Date(USA_EX$Date , format = "%m/%d/%y") 

# convert to long
USA_EX_long <- reshape2::melt(USA_EX, id.vars = c("Date"), 
                                variable.name = "Station")

# add year column
USA_EX_long$year <- year(USA_EX_long$Date)

# Summarize total days (excludes NA), and number of events
USA_EX_count <- USA_EX_long %>% 
  filter(!is.na(value)) %>%
  group_by(Station, year) %>% 
  summarise( 
    n = n(),
    count = sum(value))

write.csv(USA_EX_count,"usa extreme count.csv")

USA_EX_avgs <- USA_EX_count %>% 
  group_by(year) %>% 
  summarise( 
    n = n(),
    min = min(count),
    max = max(count),
    median = median(count),
    mean = mean(count),
    sd = sd(count))
