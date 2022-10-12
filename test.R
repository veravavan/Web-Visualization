# point1 lon 15.75 lat 50.75
# point2 lon 12.74 lat 49.74
# point3 lon 18.04 lat 59.99
# vars total_precipitation tp, 2m_temperature t2m, surface runoff sro

library(ggplot2)
library(data.table)

#data setup
point1_tp <- fread("point1_tp.csv")
point2_tp <- fread("point2_tp.csv")
point3_tp <- fread("point3_tp.csv")

tp <- merge(point1_tp, point2_tp, by="time")
tp <- merge(tp, point3_tp, by="time")
tp$Date <- as.Date(tp$time, format = '%d%m%Y')

tp[, c("time", "realization.x", "experimentVersionNumber.x", "realization.y", "experimentVersionNumber.y", "realization", "experimentVersionNumber")] <- NULL
setnames(tp, c("LAT1", "LON1", "TP1", "LAT2", "LON2", "TP2", "LAT3", "LON3", "TP3", "DATE"))


#ggplot
ggplot(tp, aes(x=DATE)) +                                                                     
  geom_line(aes(y=TP1, col='Point 1')) +
  geom_line(aes(y=TP2, col='Point 2')) +
  geom_line(aes(y=TP3, col='Point 3')) +
  xlab('Date') +
  ylab('Total Precipitation') +
  ggtitle('Total Precipitation Trend at 3 Points in Czech Republic') +
  theme_bw() +
  scale_color_manual(values=c("violetred4", "steelblue2", "blue4"))



#loop version
points <- c("1", "2", "3")
vars <- c("tp", "sro", "t2m")

for(var in vars) {
  point1 <- fread(paste("point1_", var, ".csv",sep=""))
  point2 <- fread(paste("point2_", var, ".csv",sep=""))
  point3 <- fread(paste("point3_", var, ".csv",sep=""))
  
  data <- merge(point1, point2, by="time")
  data <- merge(data, point3, by="time")
  data$Date <- as.Date(data$time, format = '%d%m%Y')
  
  data[, c("time", "realization.x", "experimentVersionNumber.x", "realization.y", "experimentVersionNumber.y", "realization", "experimentVersionNumber")] <- NULL
  setnames(data, c("LAT1", "LON1", paste0(var, "1"), "LAT2", "LON2", paste0(var, "2"), "LAT3", "LON3", paste0(var, "3"), "DATE"))
  
  print(
  ggplot(data, aes(x=DATE)) +                                                                     
    geom_line(aes(y=get(paste0(var, "1")), col='Point 1')) +
    geom_line(aes(y=get(paste0(var, "2")), col='Point 2')) +
    geom_line(aes(y=get(paste0(var, "3")), col='Point 3')) +
    xlab('Date') +
    ylab(var) +
    ggtitle(paste('Total ', var, ' Trend at 3 Points in Czech Republic')) +
    theme_bw() +
    scale_color_manual(values=c("violetred4", "steelblue2", "blue4"))
  )
}
