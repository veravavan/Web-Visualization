# point1 lon 15.75 lat 50.75
# point2 lon 12.74 lat 49.74
# point3 lon 18.04 lat 59.99
# vars total_precipitation tp, 2m_temperature t2m, surface runoff sro
#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
"""
library(ggplot2)
library(data.table)

#-------------------------------------------------------------------------------
#data setup
point1_tp <- fread("point1_tp.csv")
point2_tp <- fread("point2_tp.csv")
point3_tp <- fread("point3_tp.csv")

tp <- merge(point1_tp, point2_tp, by="time")
tp <- merge(tp, point3_tp, by="time")
tp$Date <- as.Date(tp$time, format = '%d%m%Y')

tp[, c("time", "realization.x", "experimentVersionNumber.x", "realization.y", 
       "experimentVersionNumber.y", "realization", 
       "experimentVersionNumber")] <- NULL
setnames(tp, c("LAT1", "LON1", "TP1", "LAT2", "LON2", "TP2", "LAT3", "LON3", 
               "TP3", "DATE"))


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
#-------------------------------------------------------------------------------

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
  
  data$month <- month(data$DATE)  
  print(
  ggplot(data, aes(month, group=month)) +
    geom_boxplot(aes(y=get(paste0(var, "1"))), color="black") +
    geom_boxplot(aes(y=get(paste0(var, "2"))), color="blue") +
    geom_boxplot(aes(y=get(paste0(var, "3"))), color='red') +
    xlab("month") +
    scale_x_discrete(labels=month)+
    ylab(var) +
    ggtitle(paste('Total ', var, ' Trend at 3 Points in Czech Republic')) +
    theme_bw()
    
  )
}

data$month <- month(data$DATE)
ggplot(data, aes(month, group=month)) +
  geom_boxplot(aes(y=t2m1), color="black") +
  geom_boxplot(aes(y=t2m2), color="blue") +
  geom_boxplot(aes(y=t2m3), color='red')

#v2-----------------------------------------------------------------------------
points <- c("1", "2", "3")
vars <- c("tp", "sro", "t2m")

for(var in vars) {
  data <- NULL
  for(point in points) {
      point <- fread(paste("point", point, "_", var, ".csv",sep=""))
      if(is.null(data)) {
        data <- point
      } else {
      data <- merge(data, point, by="time")        
      }
  }
  data$Date <- as.Date(data$time, format = '%d%m%Y')
  data[, c("time", "realization.x", "experimentVersionNumber.x", 
           "realization.y", "experimentVersionNumber.y", "realization", 
           "experimentVersionNumber")] <- NULL
  setnames(data, c("LAT1", "LON1", paste0(var, "1"), "LAT2", "LON2", 
                   paste0(var, "2"), "LAT3", "LON3", paste0(var, "3"), "DATE"))
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
    data$month <- month(data$DATE)  
    print(
      ggplot(data, aes(month, group=month)) +
        geom_boxplot(aes(y=get(paste0(var, "1"))), color="black") +
        geom_boxplot(aes(y=get(paste0(var, "2"))), color="blue") +
        geom_boxplot(aes(y=get(paste0(var, "3"))), color='red') +
        xlab("month") +
        scale_x_discrete(labels=month)+
        ylab(var) +
        ggtitle(paste('Total ', var, ' Trend at 3 Points in Czech Republic')) +
        theme_bw()
    
    )
}
"""
#-------------------------------------------------------------------------------
library(data.table)
library(ggplot2)

points <- c("p1", "p2", "p3")
vars <- c("tp", "sro", "t2m")
var_n <- c("precipitation", "surface runoff", "temperature")

data <- NULL
for(var in 1:3) {
  data_var <- NULL
  for(point in 1:3) {
    pointd <- fread(paste("point", point, "_", vars[var], ".csv",sep=""))
    pointd$time <- as.Date(pointd$time, format = '%d%m%Y')
    pointd$location <- as.factor(points[point])
    pointd[, c("experimentVersionNumber","realization", "lat", "lon")] <- NULL
    if(is.null(data_var)) {
      data_var <- pointd
    } else {
      data_var <- rbind(data_var, pointd)        
    }
  }
  if(is.null(data)) {
    data <- data_var
  } else {
    data <- cbind(data, data_var)        
  }
}

data_melt = melt(data, id.vars = c("time", "location"), measure.vars = vars)
data_melt$month = format(data_melt$time, "%m")
