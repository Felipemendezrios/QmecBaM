library(dplyr)

# Output directory
dir.output=file.path(getwd(),'data')
dir.calibration=file.path(getwd(),'data-raw','Seine_Aval')

load(file.path(dir.calibration,'input_h_Q_Qmec_Seine.Rdata'))

# Separate stage record from ADCP discharge measurements
# Oissel gauge station (upstream):
h_upstream=obs_t[,c('time',
                    'h_Oissel')]

colnames(h_upstream) <- c('date','h')

# Petit-Couronne gauge station  (downstream):
h_downstream=obs_t[,c('time',
                      'h_Petit_Couronne')]

colnames(h_downstream) <- c('date','h')

# Get ADCP Discharge measurements
ADCP.Q.measurements.temps <- obs_t[,c('time',
                                      'Q_Rouen',
                                      'uQ_Rouen')]
ADCP.Q.measurements=ADCP.Q.measurements.temps[!is.na(ADCP.Q.measurements.temps$Q_Rouen),]

colnames(ADCP.Q.measurements) <- c('date','Q','u_Q')
# Initial date to capture the tide
date.start <- as.POSIXct("2015-09-28 00:00:00",tz = "UTC")

# Final date to capture the tide
date.end <- as.POSIXct("2015-10-01 00:00:00",tz = "UTC")

# Pre-processing: stage record

all.data=list(h1=h_upstream,h2=h_downstream)
data=list(h1=data.frame(),h2=data.frame())

# Interpolation setting and discretization
interpolation.input='cubic'
interpolation.model='linear'
dt.stage.record=60     # in minutes
dt.model = 1           # in minutes


# Pre-treatment of input data
for(i in 1:length(all.data)){

  data[[i]]<- all.data[[i]][dplyr::between(all.data[[i]]$date,
                                           date.start,
                                           date.end),]

  # Add uncertainty at gauge station. Assumption negligible
  data[[i]]$u.h <- 0

}

# Merge results from water level measurements
data.wl <- merge(data$h1,data$h2,by=c('date'),suffixes =c('1','2'))
data.wl <- data.wl %>% arrange(date)

# Model environment

# Interpolation if time step of stage data is higher than model's
inter.required =any(dt.stage.record!=dt.model)
if(inter.required==T){
  # interpolation.model to get a time step defined at dt.model
  time.inter.model =seq(min(data.wl$date),
                        max(data.wl$date),
                        by=dt.model*dt.stage.record)

  if(interpolation.model=='cubic'){
    h1=spline(x=data.wl$date,y=data.wl$h1,method = "fmm",xout=time.inter.model)
    h2=spline(x=data.wl$date,y=data.wl$h2,method = "fmm",xout=time.inter.model)
  }else if(interpolation.model=='linear'){
    h1=approx(x=data.wl$date,y=data.wl$h1,xout=time.inter.model)
    h2=approx(x=data.wl$date,y=data.wl$h2,xout=time.inter.model)
  }

  # Rebuild data frame with observations interpolated
  data.model.inter = data.frame(date=time.inter.model,
                                h1=h1$y,
                                u.h1=0,
                                h2=h2$y,
                                u.h2=0)

}else{
  data.model.inter = data.wl
}

# Merge water level and discharge measurements (if)
Lower_Seine_Rouen <- merge(data.model.inter,
                           ADCP.Q.measurements,
                           by = c("date"),
                           all.x = T,
                           all.y = F)

# Replace NA by -9999 for BaM! (manage of missing values)
Lower_Seine_Rouen[is.na(Lower_Seine_Rouen)] <- -9999

save(Lower_Seine_Rouen,file='data/Lower_Seine_Rouen.RData')
