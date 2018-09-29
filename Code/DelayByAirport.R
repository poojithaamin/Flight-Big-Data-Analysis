
library(dplyr)
library(maps)
library(ggplot2)
library(data.table)
flights <- fread("airline_data.csv", sep=",",
                 header=TRUE,
                 stringsAsFactors=FALSE)
airport_codes <- fread("airport_codes.csv", col.names=c("Origin", "OriginState"),
                       stringsAsFactors = FALSE) 
flights <- left_join(flights, airport_codes, by="Origin")
# Create subset of data containing origin state and arrival delay
subset_state <- select(flights, OriginState, ArrDelay) 
subset_state <- subset_state[!is.na(subset_state$ArrDelay),] 
subset_state <- group_by(subset_state, OriginState)
subset_summary <- summarise(subset_state, AveDelay=mean(ArrDelay))
# Create graphic of US States colored by average delay time
map = map_data("state")

ggplot(subset_summary, aes(fill=AveDelay)) +
  geom_map(aes(map_id=OriginState), map=map) +
  scale_fill_distiller(name = "Average Delay (mins)", palette = "Spectral", direction=-1) +
  geom_text(aes(label = OriginState)) +
  expand_limits(x=map$long, y=map$lat) +
  theme_void() +
  labs(title = "Average Flight Arrival Delay by State")
ggsave("ave_delay_by_state.jpg", width=9, height=5)



