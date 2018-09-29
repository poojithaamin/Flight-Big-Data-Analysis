
library(dplyr)
library(maps)
library(ggplot2)
library(data.table)
flights1 <- fread("airline_data.csv", sep=",",
                  header=TRUE,
                  stringsAsFactors=FALSE)
flights2 <- fread("2006.csv", sep=",",
                  header=TRUE,
                  stringsAsFactors=FALSE)
flights3 <- fread("2007.csv", sep=",",
                  header=TRUE,
                  stringsAsFactors=FALSE)
flights = rbind(flights1, flights2,flights3 )
# Convert Months from number to factor
flights$Month <- factor(flights$Month)
levels(flights$Month) <- month.abb
subset_month <- select(flights, Month, ArrDelay)
ggplot(subset_month, aes(Month,ArrDelay, fill=factor(Month))) + 
  geom_violin(aes(group=Month)) +
  theme(legend.position="none") +
  labs(y = "Arrival Delay (in minutes)") +
  labs(title = "Average Flight Arrival Delay by Month") 
ggsave("ave_delay_by_month.jpg", width=9, height=6)
