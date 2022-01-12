library(maptools)
library(lubridate)

# adapted from http://r.789695.n4.nabble.com/maptools-sunrise-sunset-function-td874148.html
ephemeris <- function(lat, lon, date, span=1, tz="UTC") {
  
  # convert to the format we need
  lon.lat <- matrix(c(lon, lat), nrow=1)
  
  # make our sequence - using noon gets us around daylight saving time issues
  day <- as.POSIXct(date, tz=tz)
  sequence <- seq(from=day, length.out=span , by="days")
  
  # get our data
  sunrise <- sunriset(lon.lat, sequence, direction="sunrise", POSIXct.out=TRUE)
  sunset <- sunriset(lon.lat, sequence, direction="sunset", POSIXct.out=TRUE)
  solar_noon <- solarnoon(lon.lat, sequence, POSIXct.out=TRUE)
  
  # build a data frame from the vectors
  data.frame(date=as.Date(sunrise$time),
             sunrise=format(sunrise$time, "%H%M"),
             solarnoon=format(solar_noon$time, "%H%M"),
             sunset=format(sunset$time, "%H%M"),
             day_length=as.numeric(difftime(sunset$time,sunrise$time,units="hours")))
  
}
lat=55.860916, lon=-4.251433


# these functions need the lat/lon in an unusual format
data=ephemeris(lat=-80, lon=0, date=Sys.Date(), span=365, tz="Europe/London")
data$day_length_diff_hours=diff(c(NA,data$day_length))
data$day_length_diff_min=data$day_length_diff*60
data
plot(data$sunset~data$date)



fig <- plot_ly(data,  type = 'scatter', mode = 'lines+markers', fill = 'tozeroy')%>%
  add_trace(x = ~date, y = ~day_length, name = '',
            hovertemplate = paste0("<br>Date: ",data$date,
            paste('<br>Daylight hours : %{y:.1f} hours'),
            paste0("<br>Sunrise: ",data$sunrise),
            paste0("<br>Sunset: ",data$sunset)))%>%
  
  layout(showlegend = F)%>%layout(
    title = "Daylight hours",
    xaxis = list(
      rangeselector = list(
        buttons = list(
          list(
            count = 3,
            label = "3 mo",
            step = "month",
            stepmode = "todate"),
          list(
            count = 6,
            label = "6 mo",
            step = "month",
            stepmode = "todate"),
          list(
            count = 1,
            label = "1 yr",
            step = "year",
            stepmode = "todate"),
          list(
            count = 1,
            label = "YTD",
            step = "year",
            stepmode = "todate"),
          list(step = "all"))),
      
      rangeslider = list(type = "date")),
    
    yaxis = list(title = "Daylight hours"))
fig
