# Plotting and summarizing VA and USA heat days
library(ggplot)
library(reshape2)
library(dplyr)
library(patchwork)
library(lubridate)

# 1. VA heat events ####
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
combo <- VA_station_plot | (VA_total_plot / VA_total_boxplot ) 
# add letter
combo_va <- combo + plot_annotation(tag_levels = 'A')

# Save figure
ggsave("VA Heat Events.png", plot = combo_va, device = "png", path = NULL, 
       scale = 1, width = 8.27, height = 6.54, units = "in", dpi = 800)


# 2. VA extreme heat events  #####
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

# 3. USA heat events ####
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

# 4. USA extreme heat events ####
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
