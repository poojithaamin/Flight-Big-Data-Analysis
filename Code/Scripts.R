library(plyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(dplyr)
# Change working directory to where the 2013flights.Rdata file is located:
setwd("/Users/poojithaamin/Downloads")
load(file="2013flights.Rdata")
df = df %>%
  mutate(
    day_of_week=factor(day_of_week,levels=c(1:7,9),
                       labels=
                         c("Monday",
                           "Tuesday",
                           "Wednesday",
                           "Thursday",
                           "Friday",
                           "Saturday",
                           "Sunday",
                           "Unknown"
                         )),
    month=substr(date,6,7),
    datenum=substr(date,9,10),
    departure_hour = round(departure_hour/100,0),
    delay=ifelse(delay<0,0,delay),
    arr_delay=ifelse(arr_delay<0,0,arr_delay)) %>%
  filter(departure_hour > 5 & departure_hour< 24)  
### PLOT RESULTS JUST FOR "SMF","SFO"
plot_data = df %>%
  filter(airport %in% c("SMF","SFO")) %>%
  group_by(departure_hour,airport) %>%
  dplyr::summarise(mu=mean(delay,na.rm=TRUE),
                   se=sqrt(var(delay,na.rm=TRUE)/length(na.omit(delay))),
                   obs=length(na.omit(delay)))
p=ggplot(plot_data,aes(x=departure_hour,y=mu,min=mu-se,max=mu+se,group=airport,color=airport)) +
  geom_line(lwd = 1.5) +
  geom_point() +
  geom_errorbar(width=.33) +
  scale_x_continuous(breaks=seq(5,23)) +
  labs(x="Hour of Day",y="Average Departure Delay",title="Flight Delays by Departure Time and Airport") +
  theme(legend.position="bottom") +
  scale_color_discrete(name="Delay Type")
p



### LOOKING AT THE DISTRIBUTION. 12 Noon in SMF:
plot_data = df %>%
  filter(airport == "SMF" & departure_hour == 12)
plot_data = na.omit(plot_data)
p1<-ggplot(plot_data, aes(x=delay)) + 
  geom_histogram(aes(y = ..density..), color="black", fill="NA") +
  geom_density(color="blue", fill = "blue", alpha = 0.3)+ xlim(0, 60) +
  labs(x="Departure Delay",y="Density",title="8AM Departure Delay Distribution - SMF")
p1
### ADDED JITTER PLOTS. FIRST SMF:
plot_data = df %>%
  filter(airport == "SMF") %>%
  group_by(departure_hour,airport)
plot_data = na.omit(plot_data)
b1<-ggplot(plot_data, aes(departure_hour, delay)) + ylim(0, 600) +
  geom_jitter(alpha=I(1/4), col = "blue") +
  theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(5,23)) +
  labs(x="Hour of Day",y="Departure Delay",title="Flight Delays by Departure Time - SMF")
b1
### ADDED JITTER PLOTS. SFO:
plot_data = df %>%
  filter(airport == "SFO") %>%
  group_by(departure_hour,airport)
plot_data = na.omit(plot_data)
b1<-ggplot(plot_data, aes(departure_hour, delay)) + ylim(0, 600) +
  geom_jitter(alpha=I(1/4), col = "red") +
  theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(5,23)) +
  labs(x="Hour of Day",y="Departure Delay",title="Flight Delays by Departure Time - SFO")
b1