library("sparklyr")
library("yarrr")
library("dplyr")
sc <- spark_connect(master = "local")
df <- spark_read_csv(sc, name = "airdata_2015", path = "/dataset/airline-data.2005-2015/airline-data/On_Time_On_Time_Performance_2005_1.csv", header = TRUE, delimiter = "," )
df %>% group_by(Carrier) %>% summarise(DepDelay = mean(DepDelay), ArrDelay = mean(ArrDelay))
pirateplot(formula = ArrDelay ~ Carrier,
            data = df,
            point.col = rainbow(19),
            point.pch = 21,
            point.cex = .7,
            main = "Arrival Delay ",
            xlab = "Carrier",
            ylab = "Arrival Delay in minutes")
pirateplot(formula = DepDelay ~ Carrier,
           data = df,
           point.col = rainbow(19),
           point.pch = 21,
           point.cex = .7,
           main = "Departure Delay ",
           xlab = "Carrier",
           ylab = "Departure Delay in minutes")
