library(dplyr)

# Output directory
dir.output=file.path(getwd(),'data')
dir.calibration=file.path(getwd(),'data-raw','Saint_Laurent')

# Read the stage record of the two stations:

# Lauzon gauge station  (downstream):
h_Lauzon=data.frame(data.table::fread(file.path(dir.calibration,
                                                'water_level_1962_2019',
                                                'h_Lauzon',
                                                '3250-01-JAN-2000_slev.csv'),
                                      skip=8))

colnames(h_Lauzon) <- c('year','month','day','hour','minute','h')

# Neuville gauge station (upstream):
h_Neuville=data.frame(data.table::fread(file.path(dir.calibration,
                                                  'water_level_1962_2019',
                                                  'h_Neuville',
                                                  '3280-01-JAN-2000_slev.csv'),
                                        skip=8))

colnames(h_Neuville) <- c('year','month','day','hour','minute','h')

# Initial date to capture the tide
date.start <- data.frame('year'=2009,
                         'month'=8,
                         'day'=20,
                         'hour'=0,
                         'minute'=0,
                         'second'=0)

# Final date to capture the tide
date.end <- data.frame('year'=2009,
                       'month'=8,
                       'day'=22,
                       'hour'=0,
                       'minute'=0,
                       'second'=0)


# Get discharge measurements:
Q.station <- 'Q_ADCP_Saint-Nicolas_F'

# Pre-processing: stage record

all.data=list(h1=h_Neuville,h2=h_Lauzon)
data=list(h1=data.frame(),h2=data.frame())

# Interpolation setting and discretization
interpolation.input='cubic'
interpolation.model='linear'
dt.stage.record=60     # in minutes
dt.model = 1           # in minutes

# Pre-treatment of input data
for(i in 1:length(all.data)){

  all.data[[i]] <- data.frame(all.data[[i]][,1:5],
                              second=rep(0,nrow(all.data[[i]])),
                              h=all.data[[i]][,6])
  # transform and add date in a different format
  all.data[[i]]$date=as.POSIXct(paste(all.data[[i]]$year,
                                      all.data[[i]]$month,
                                      all.data[[i]]$day,
                                      all.data[[i]]$hour,
                                      all.data[[i]]$minute,
                                      all.data[[i]]$second), format="%Y %m %d %H %M %S")

  # Water level measurement during period a specific period of time
  data[[i]]<- all.data[[i]][dplyr::between(all.data[[i]][,1:6],
                                           date.start[,1:6],
                                           date.end[,1:6]),]

  # Interpolation input if NA detected
  gap=any(as.numeric(diff(data[[i]]$date),units='mins')!=dt.stage.record)
  if(gap==TRUE){
    time.inter.input.data = seq(data[[i]]$date[1],
                                data[[i]]$date[nrow(data[[i]])],by=dt.stage.record*(60))
    if(interpolation.input=='cubic'){
      h=spline(x=data[[i]]$date,y=data[[i]]$h,method = "fmm",xout=time.inter.input.data)
    }else if(interpolation.input=='linear'){
      h=approx(x=data[[i]]$date,y=data[[i]]$h,xout=time.inter.input.data)
    }
    # Rebuild data frame with input data interpolated
    data[[i]] = data.frame(year=year(time.inter.input.data),
                           month=month(time.inter.input.data),
                           day=mday(time.inter.input.data),
                           hour=hour(time.inter.input.data),
                           minute=minute(time.inter.input.data),
                           second=second(time.inter.input.data),
                           h=h$y,
                           date=time.inter.input.data,
                           u.h=0)
  }else{

    # Add uncertainty at gauge station. Assumption negligible
    data[[i]]$u.h <- 0
  }
}

# Merge results from water level measurements
data.wl <- merge(data$h1,data$h2,by=c('year','month','day','hour','minute','second','date'),suffixes =c('1','2'))
data.wl <- data.wl %>% arrange(date)

# Model environment

# Interpolation if time step of stage data is higher than model's
inter.required =any(dt.stage.record!=dt.model)
if(inter.required==T){
  # interpolation.model to get a time step defined at dt.model
  time.inter.model =seq(data.wl$date[1],
                        data.wl$date[nrow(data.wl)],by=dt.model*dt.stage.record)

  if(interpolation.model=='cubic'){
    h1=spline(x=data.wl$date,
              y=data.wl$h1,
              method = "fmm",
              xout=time.inter.model)
    h2=spline(x=data.wl$date,
              y=data.wl$h2,method = "fmm",
              xout=time.inter.model)
  }else if(interpolation.model=='linear'){
    h1=approx(x=data.wl$date,
              y=data.wl$h1,
              xout=time.inter.model)
    h2=approx(x=data.wl$date,
              y=data.wl$h2,
              xout=time.inter.model)
  }

  # Rebuild data frame with observations interpolated
  data.model.inter = data.frame(year=data.table::year(time.inter.model),
                                month=data.table::month(time.inter.model),
                                day=data.table::mday(time.inter.model),
                                hour=data.table::hour(time.inter.model),
                                minute=data.table::minute(time.inter.model),
                                second=data.table::second(time.inter.model),
                                date=time.inter.model,
                                h1=h1$y,
                                u.h1=0,
                                h2=h2$y,
                                u.h2=0)
}else{
  data.model.inter = data.wl
}

# Pre-processing: discharge measurements
ADCP.Q.measurements <-   read.table(file.path(dir.calibration,
                                              'Discharge',
                                              paste0(Q.station,'.txt')),
                                    header = TRUE)

colnames(ADCP.Q.measurements) <- gsub("s","",colnames(ADCP.Q.measurements))
colnames(ADCP.Q.measurements) [6] <- 'second'

# Merge water level and discharge measurements (if)
Saint_Laurent_F2 <- merge(data.model.inter,
                          ADCP.Q.measurements,
                          by = c("year", "month", "day", "hour", "minute", "second"),
                          all.x = T,
                          all.y = T)

# Replace NA by -9999 for BaM! (manage of missing values)
Saint_Laurent_F2[is.na(Saint_Laurent_F2)] <- -9999

# Transfer of calibration data: stage record at the two stations and ADCP discharge measurements.
save(Saint_Laurent_F2,file='data/Saint_Laurent_F2.RData')
