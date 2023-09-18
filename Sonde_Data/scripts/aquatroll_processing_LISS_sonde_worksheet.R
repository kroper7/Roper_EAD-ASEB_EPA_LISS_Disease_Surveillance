
###--------- R packages
require(gsw)
require(lubridate)
require(magrittr)
require(NISTunits)
require(readr)
require(rvest)
require(tools)
library(sjmisc)   

###--------- R version check
r <- R.version
if(as.numeric(r$major)<4 & as.numeric(r$minor)<1){
  warning(paste('\n\n Functions created for R version 4.1.0 \n\n 
                You are running an older version; some compatability issues may exist! \n\n'),
          immediate. = T)
}
rm(r)


aquatroll_fields_LISS <- function(){ # LISS Sonde data; updated 2023/09/08
  c("Date Time",                       
    "Actual Conductivity (µS/cm)",      
    "Specific Conductivity (µS/cm)",    
    "Salinity (PSU)",                  
    "Resistivity (Ω⋅cm)",              
    "Density (g/cm³)",                  
    "Total Dissolved Solids (ppt)",     
    "RDO Concentration (mg/L)",        
    "RDO Saturation (%Sat)",            
    "Oxygen Partial Pressure (Torr)",   
    "Chlorophyll-a Fluorescence (RFU)", 
    "pH (pH)",                         
    "pH mV (mV)",                       
    "ORP (mV)",                         
    "Temperature (°C) AT",              
    "External Voltage (V)",            
    "Battery Capacity (%)",             
    "Barometric Pressure (mm Hg)")
}
input <- raw_Sonde_rootdir

###--------- summary_aquatroll
# summary_aquatroll <- function(input, # htm or csv file that contains the raw aquatroll output
#                               ignore.marked = T, # TRUE will ignore the marked column from being exported in the output
#                               flds_req = aquatroll_fields_v2() # fields required for exporting raw data; default to newer set (2022/11/18)
# )
# input <- 'C:/Users/samjg/Documents/Github_repositories/LISS_sonde_data/data/csv/0723ASHC_Sonde.csv'
summary_aquatroll <- function(input, # htm or csv file that contains the raw aquatroll output
                              ignore.marked = T, # TRUE will ignore the marked column from being exported in the output
                              flds_req = aquatroll_fields_LISS()) # fields required for exporting raw data; default to newer set (2022/11/18)
  
{ # start the function
  # if not a csv or htm then get outta here bro
  if(file_ext(input)!='csv' & file_ext(input)!='htm'){
    warning(paste('\n\n File format needs to be csv or htm! \n\n'),
            immediate. = T)
  }
  if(file_ext(input)=='csv'){ # if csv  call the file and whatnot
    D <- readLines(input)
    # get header row of data
    ind <- grep('Date Time',D)
    # extract data
    D <- read_csv(input,skip=ind-1,col_types = cols())
    columns <- names(D)
    H <- read.csv(input,header=F,nrows=ind-1)
  }
  # if(file_ext(input)=='htm'){ # for input htm files, lets not worry about this bcus we only have csvs for LISS
  #   H <- read_html(input) %>%
  #     html_table(fill = T) %>%
  #     .[[1]]
  #   # get header row of data
  #   row_d <- which(H[,1] == "Date Time")
  #   columns <- H[row_d,]
  #   # extract data
  #   D <- H[(row_d+1):nrow(H),]
  #   names(D) <- as.character(columns)
  # }
  H_df <- data.frame(H) # as data frame it
  D_df <- data.frame(D) # as data frame it
  # D[2:(ncol(D-2))] <- lapply(D[2:(ncol(D-2))],as.numeric) # other version csv files have last column as just NAs, they omitted them here ad D-2
  D_df[2:(ncol(D_df))] <- lapply(D_df[2:(ncol(D_df))],as.numeric) # first two columns are date and time, start at first contianing data 
  
  ### strip serial numbers - LISS HAS SEVERAL DIFFERENT SERIEAL NUMBERS!!!
  names(D_df) <- gsub(" \\([0-9]+\\)", "", columns) # ommit the numeric information from all column names 
  
  ### Aquatroll serial number
  # row_h <- which(H[,1] == "Device Model = Aqua TROLL 600 Vented")
  row_h        <- which(H_df[,1] == "Device Model = Aqua TROLL 600") # LISS data does not have 'vented' in the serial title
  aquatroll_sn <- as.numeric(strsplit(as.character(H_df[row_h+1,1]),'=')[[1]][2]) # the row after Device model is "Device SN = <its here>", grab the serial number
  ### Handheld serial number
  row_sn <- grep('Device Model',H_df[,1]) # theres two row names that have this delimier string (Device Model = In-Situ Bluetooth Device" AND "Device Model = Aqua TROLL 600 Vented" )
  
  # LISS Sonde data does not have indication of other sn in the H row set.. find a way to call these
  # row_h2 <- setdiff(row_sn,row_h) 
  # handheld_sn <- ifelse(length(row_h2)>0,
  #                       as.numeric(strsplit(as.character(H[row_h2+1,1]),'=')[[1]][2]),
  #                       NA) # calls the device SN as Device SN = 674191
  
  
  
  ### rename temperaures - NOTE THIS ASSUMES THAT THIS SEREIAL NUMBER IS JUST FOR AQUATOLL?
  ind_aquatroll <- grep(aquatroll_sn,columns) # calls all columns  that have the serieal number delimiter aquatroll_sn
  
  
  ### Aquatroll temperature
  ind_temp <- grep('Temperature',columns) %>%
    intersect(ind_aquatroll)
  names(D_df)[ind_temp] <- paste(names(D_df)[ind_temp],'AT') # somply inserts 'AT' to the temperature column name
  
  ### Handheld temperature - DOES NOT APPLY TO LISS SONDE DATA  
  # ind_temp2 <- grep('Temperature',columns) %>%
  #   setdiff(ind_aquatroll)
  # names(D)[ind_temp2] <- paste(names(D)[ind_temp2],'HH')
  
  
  ### what are depth units? - DOES NOT APPLY TO LISS SONDE DATA 
  # depth_unit <- substr(names(D)[grep('Depth',names(D))],7,20)
  ### Strip depth units
  # names(D)[grep('Depth',names(D))] <- 'Depth'
  
  ### missing fields, ny fields they mean columns, lets not get crazy here
  flds_miss <- setdiff(flds_req, names(D_df)) # flds_req is in the original summary_aquatoll call - we call the preexisting LISS version
  
  ### datetime - We can do this
  if(gsub(".*/","",input) !='0723ASHC_Sonde.csv') {
    D_df[,1] <- with_tz(force_tz(ymd_hms(D_df[,1]),tz='America/New_York'),'UTC')
     } else ( D_df[,1] <- with_tz(force_tz(mdy_hm(D_df[,1]),tz='America/New_York'),'UTC') # final rows as 'Log notes, Started, Paused, etc. when taking down are now NAs
              )
  
  D_om     <- D_df %>% dplyr::filter(!`Date Time` %in% NA) # ommit htese final rows to computer the whole run time, otherwise  error hits an NA
  # total duration
  dtime <- D_om[1,1] ### start time
  etime <-D_om[nrow(D_om)-1,1]
  tail(D_om)
  duration_total_secs <- as.numeric(D_om[nrow(D_om),1]-D_om[1,1],units='secs')+1
  duration_total_hrs  <- duration_total_secs/3600
  duration_total_days <- duration_total_hrs/24

  
  ### ommit pre and post deployment data based on salinity
  sal_outliers              <- D_om %>% dplyr::filter(`Salinity (PSU)` < 1)
  n_outlier                 <- nrow(sal_outliers)
  outlier_timestamps        <- list((D_om %>% dplyr::filter(`Salinity (PSU)` %in%  sal_outliers$`Salinity (PSU)`))$`Date Time`)
  outlier_timestamps_nested <- paste(as.character(interaction(outlier_timestamps,sep=",")))



  ### lat/lon - DOES NOT APPLY TO LISS SONDE DATA 
  # if(length(grep("latitude",columns,ignore.case = T))>0){
  #   lat_avg <- mean(D[,grep("latitude",columns,ignore.case = T)],na.rm=T)
  #   lon_avg <- mean(D[,grep("longitude",columns,ignore.case = T)],na.rm=T)
  # } else {
  #   lat_avg <- lon_avg <- NA
  # }
  
  
  ### max depth - DOES NOT APPLY TO LISS SONDE DATA 
  # z_max <- max(D[,grep('depth',columns,ignore.case = T)],na.rm=T)
  
  
  ### min oxygen - We can do this
  do_min      <- min(D_om[,grep('RDO Concentration',columns,ignore.case = T)],na.rm=T) # minimum oxygen 
  do_min_time <- (D_om %>% dplyr::filter(`RDO Concentration (mg/L)` %in% do_min))$`Date Time`
    
  ### column name 'Marked is there data dropout -  - DOES NOT APPLY TO LISS SONDE DATA 
  # if(ignore.marked==T){
  #   M <- D[,-grep('marked',columns,ignore.case = T)]
  # }
  # missingness <- unlist(lapply(M,function(x) sum(is.na(x))))
  # columns <- names(D)
  # dropout <- columns[which(missingness>0)]
  
  ### number of samples
  n_samp <- nrow(D_om)


  
  ### save output
  out <- data.frame(Input=gsub(".*/","",input), # just call the file not the root dir
                    Aquatroll_sn=aquatroll_sn,
                    Start_time_utc=dtime,
                    End_time_utc=etime,
                    Duration_days=duration_total_days,
                    #handheld_sn=handheld_sn,
                    Num_samp=n_samp,
                    #z_max_m=z_max,
                    Oxygen_min=do_min,
                    PSU_under_1=n_outlier, # based on salinity < 1
                    PSU_outlier_timestamps=outlier_timestamps_nested,#list(sal_outlier_timestamps), # list of timestamps where sal is < 1
                    #dz_dt=dz_dt,
                    #lon_dd=lon_avg,
                    #lat_dd=lat_avg,
                    flds_miss=toString(flds_miss) #,
                    # dropout=toString(dropout)
                    )
  # return(out)
  return(out %>%
           dplyr::group_by(across(c(-PSU_outlier_timestamps))) %>%
           dplyr::summarise(PSU_outlier_timestamps = toString(PSU_outlier_timestamps), .groups = 'drop'))
}


# For loop for the summary_aquatroll
cumulative.summary.table <- data.frame() # start dataframe 
df.loop                  <- data.frame(matrix(nrow = 1, ncol = 10)) # create dataframe to save cumunalitively during for loop
colnames(df.loop)        <- c('Input', 
                              'Aquatroll_sn', 
                              'Start_time_utc', 
                              'End_time_utc' , 
                              'Duration_days', 
                              'Num_samp',
                              'Oxygen_min',
                              'PSU_under_1',
                              'PSU_outlier_timestamps',
                              'flds_miss') # names in the for loop

path.p              <- "C:/Users/samjg/Documents/Github_repositories/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/" #the location of all your respirometry files 
file.names.table    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',sep=''), pattern = "csv$", recursive = TRUE)))) 

for (m in 1:nrow(file.names.table)) {
  raw_Sonde_rootdir        <- paste(path.p,'/',file.names.table[m,1], sep='') #reads in the data files
  df.loop                  <- as.data.frame(summary_aquatroll(raw_Sonde_rootdir))
  cumulative.summary.table <- rbind(cumulative.summary.table, df.loop) #bind to a cumulative list dataframe
  }

write.csv(cumulative.summary.table, "C:/Users/samjg/Documents/Github_repositories/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/summary_aquatroll.csv")

