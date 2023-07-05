require(magrittr)
require(lubridate)
require(dplyr)
library(fst)
library(data.table)
library(naniar)

# set directory to my project folder
dir_data <- "/n/filepath/myfolder"

# set years of data, load and combine adrd hospitalizations
years <- 2000:2016
cases <- rbindlist(lapply(years, function(y) {
  read_fst(paste0(dir_data, "hosp_denom/", y, ".fst"), 
           as.data.table = TRUE) # read in hospitalization data 
}))
cases[ADRD_any == TRUE, .N]


#restrict to first ADRD event
cases_2 <- cases[, .SD[ADATE == min(ADATE)][1], by = QID] 

cases_2[ADRD_any == TRUE, .N]
rm(cases);gc()

#create control days 
for (i in 1:6){ 
  tmp_back = cases_2 #create copy of cases dataframe to have "backwards" controls
  tmp_forw = cases_2#create copy of cases dataframe to have "forward" controls
  
  # Going forward by i weeks and backward by i weeks  
  tmp_back$ADATE = tmp_back$ADATE - 7*i
  tmp_forw$ADATE = tmp_forw$ADATE + 7*i
  
  # Keep the matched control that has the same month as the case
  tmp_back$month_new = month(tmp_back$ADATE)
  tmp_back = tmp_back[tmp_back$month == tmp_back$month_new, ]
  tmp_back$month_new = NULL # Trash it
  
  tmp_forw$month_new = month(tmp_forw$ADATE)
  tmp_forw = tmp_forw[tmp_forw$month == tmp_forw$month_new, ]
  tmp_forw$month_new = NULL # Trash it
  
  # Combine eligible controls 
  tmp = bind_rows(tmp_back, tmp_forw) 
  if (i==1){
    control = tmp
  }else{
    control = bind_rows(control, tmp)
  }
  rm(tmp_back);rm(tmp_forw); rm(tmp);gc()
  cat(i)
}

#create case/control variables 
control$case = 0
cases_2$case = 1


# Combine cases and controls
ADRD_CCO = bind_rows(cases_2, control)
#table(ADRD_CCO$case)  #3.4 control days per case 



# Merging exposures and confounder in 1 dataset
setDT(ADRD_CCO)
ADRD_CCO[, date := ADATE]
setkey(ADRD_CCO, zip, date)
zips <- unique(cases_2$zip)

rm(control);gc()
rm(cases_2);gc()

# PM2.5
pm25 <- fread(paste0("/n/filepath/",
                     "zipcode/daily/",
                     "all_days_PM.csv"), sep = ",")
pm25[, date := ymd(date)]
pm25 <- pm25[year(date) %in% years & ZIP %in% zips]
setnames(pm25, c("zip", "pm25_lag_0", "date"))
setkey(pm25, zip, date)
pm25[, paste0("pm25_lag_", 1:14) := shift(.SD, 1:14, type = "lag"),
     by = "zip", .SDcols = "pm25_lag_0"]
ADRD_CCO <- merge(ADRD_CCO, pm25, by = c("zip", "date"), all.x = TRUE)
rm(pm25); gc()
pct_miss(ADRD_CCO)

# NO2
no2 <- fread(paste0("/n/filepath/",
                    "zipcode/daily/",
                    "all_days_NO2.csv"), sep = ",")
no2[, date := ymd(date)]
no2 <- no2[year(date) %in% years & ZIP %in% zips]
setnames(no2, c("zip", "no2_lag_0", "date"))
setkey(no2, zip, date)
no2[, paste0("no2_lag_", 1:14) := shift(.SD, 1:14, type = "lag"),
    by = "zip", .SDcols = "no2_lag_0"]
ADRD_CCO <- merge(ADRD_CCO, no2, by = c("zip", "date"), all.x = TRUE)
pct_miss(no2)
pct_miss(ADRD_CCO)
rm(no2); gc()


# Ozone
ozone <- fread(paste0("/n/filepath/",
                      "daily/zipcode/",
                      "ozone.csv"), sep = ",")
ozone[, date := ymd(date)]
ozone <- ozone[year(date)  %in% years & ZIP %in% zips]
setnames(ozone, c("zip", "ozone_lag_0", "date"))
setkey(ozone, zip, date)
ozone[, paste0("ozone_lag_", 1:14) := shift(.SD, 1:14, type = "lag"),
      by = "zip", .SDcols = "ozone_lag_0"]
ADRD_CCO <- merge(ADRD_CCO, ozone, by = c("zip", "date"), all.x = TRUE)
rm(ozone); gc()


# Heat Index 
hi<- read_fst(paste0("heat_index.fst"), as.data.table = TRUE)
hi[, date := ymd(date)]
hi <- hi[year(date) == year & zip %in% zips]
setnames(hi, c("zip", "date", "hi_lag_0"))
setkey(hi, zip, date)
hi[, paste0("hi_lag_", 1:14) := shift(.SD, 1:14, type = "lag"),
   by = "zip", .SDcols = "hi_lag_0"]
ADRD_CCO <- merge(ADRD_CCO, hi, by = c("zip", "date"), all.x = TRUE)
rm(hi); gc()

#save file 
write_fst(ADRD_CCO, paste0(dir_data,'data.fst')) 

