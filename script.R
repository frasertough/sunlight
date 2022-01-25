library(maptools)
library(lubridate)
library(plotly)
ephemeris <- function(lat, lon,name="", date, span=1, tz="UTC") {
  
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
  data.frame(name=name,date=as.Date(sunrise$time),
             sunrise=format(sunrise$time, "%H%M"),
             solarnoon=format(solar_noon$time, "%H%M"),
             sunset=format(sunset$time, "%H%M"),
             day_length=as.numeric(difftime(sunset$time,sunrise$time,units="hours")))
  
}
create_buttons <- function(df, y_axis_var_names) {
  lapply(
    y_axis_var_names,
    FUN = function(var_name, df) {
      button <- list(
        method = 'restyle',
        args = list('y', list(df[df$name==var_name,c("sunrise","sunset","day_length") ])),
        label = sprintf(var_name)
      )
    },
    df
  )
  
}

# these functions need the lat/lon in an unusual format
locations=data.frame(lat=c(56.708861334605054,
                           55.860210237849415,
                           51.51247642759147),
                     lon=c(-5.713564551768571,
                           -4.255987857346223,
                           -0.12889642997756387),
                     name=c("Ardnamurchan",
                            "Glasgow",
                            "London"))


data=rbindlist(lapply(1:nrow(locations),function(i){ephemeris(lat=locations$lat[i],
                                                              lon=locations$lon[i],
                                                              name=locations$name[i],
                                                              date=Sys.Date(), 
                                                              span=365,
                                                              tz="Europe/London")}))
data$day_length_diff_hours=diff(c(NA,data$day_length))
data$day_length_diff_min=data$day_length_diff*60

              

data=data[data$name=="Glasgow",]


fig <- plot_ly(data,  type = 'scatter', mode = 'lines+markers', fill = 'tozeroy')%>%
  add_trace(x = ~date, y = ~day_length, name = '',
            hovertemplate = paste0("<br>Date: ",data$date,
            paste('<br>Daylight hours : %{y:.1f} hours'),
            paste0("<br>Sunrise: ",data$sunrise),
            paste0("<br>Sunset: ",data$sunset)))%>%
  
  layout(showlegend = F)%>%layout(
    title = "Daylight hours (Glasgow)",   
    xaxis = list(
      rangeselector = list(
        buttons = list(
          list(step = "all",label="Reset"))),
      
      rangeslider = list(type = "date")),
    
    yaxis = list(title = "Daylight hours"))
fig





