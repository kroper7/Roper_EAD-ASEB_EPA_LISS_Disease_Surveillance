


# HYPOXIA PLOTS ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

path.p              <- "C:/Users/samuel.gurr/Documents/Github_repositories/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/Hypoxic_events" #the location of all your respirometry files 
file.names.table    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',sep=''), pattern = "Summary.csv$", recursive = FALSE)))) 


Summary.table            <- data.frame(matrix(nrow = 1, ncol = 6)) # create dataframe to save cumunalitively during for loop
colnames(Summary.table)  <- c('filename',
                              'type',
                              'threshold',
                              'numberFound',
                              'avgDurMinutes',
                              'avgAvgDO') # names in the for loop

library(dplyr)
for (m in 1:nrow(file.names.table)) {
  dat_loop              <- as.data.frame(read.csv(paste(path.p,'/',file.names.table[m,1], sep=''))) %>%  #reads in the data files
                              dplyr::mutate(filename = gsub("_Sonde.*","", file.names.table[m,1]))
  #summary_dat              <- summary_dat[!is.na(summary_dat$type),
  
  Summary.table <- rbind(dat_loop, Summary.table) #bind to a cumulative list dataframe
  # print(df_total)
}

Summary.table.om       <- Summary.table[!is.na(Summary.table$filename),] # omit NAs in filename 
Summary.table.final    <- Summary.table.om %>%
                            dplyr::rename(frequency = numberFound,
                                          duration = avgDurMinutes,
                                          magnitude = avgAvgDO) %>% 
                            dplyr::mutate(EQ = (as.numeric(frequency) * as.numeric(duration)) / as.numeric(magnitude) ) %>% 
                            mutate_all(funs(ifelse(is.na(.), 0, .)))  %>% # convert remaining NAs to 0
                            dplyr::mutate(Site = gsub(".*_","",filename),
                                          Date = gsub("_.*","",filename))
Summary.table.final  

library(ggplot2)
Duration_plot <- Summary.table.final %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(duration)/60, fill = as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("Duration of hypoxic events") +
  ylab("Duration of events (hours; minimum 1 hour)")# +
  # facet_wrap(~Date)
# Duration_plot

Frequency_plot <- Summary.table.final %>% 
  dplyr::filter(frequency >0) %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(frequency), fill = as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("Frequency of hypoxic events") +
  ylab("Number of events") #+
  # facet_wrap(~Date)
# Frequency_plot

EQ_plot <- Summary.table.final %>% 
  dplyr::filter(!EQ == 'NaN') %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(EQ), fill = as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("(F*D)/M of hypoxic events") +
  ylab("[Freq*Duration/Mean_magnitude]")#  +
  # facet_wrap(~Date) 
# EQ_plot

library(ggpubr)
pdf("C:/Users/samjg/Documents/Github_repositories/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/Hypoxic_events/2023_Summary_plots.pdf", 
    height = 12, width =10)
ggarrange(Duration_plot,
          Frequency_plot,
          EQ_plot,
          nrow=3)
dev.off()












# Acidifcation PLOTS ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::





path.p              <- "C:/Users/samuel.gurr/Documents/Github_repositories/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/Acidification_events" #the location of all your respirometry files 
file.names.table    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',sep=''), pattern = "Summary.csv$", recursive = FALSE)))) 


Summary.table            <- data.frame(matrix(nrow = 1, ncol = 6)) # create dataframe to save cumunalitively during for loop
colnames(Summary.table)  <- c('filename',
                              'type',
                              'threshold',
                              'numberFound',
                              'avgDurMinutes',
                              'avgAvgpH') # names in the for loop

library(dplyr)
for (m in 1:nrow(file.names.table)) {
  dat_loop              <- as.data.frame(read.csv(paste(path.p,'/',file.names.table[m,1], sep=''))) %>%  #reads in the data files
    dplyr::mutate(filename = gsub("_Sonde.*","", file.names.table[m,1]))
  #summary_dat              <- summary_dat[!is.na(summary_dat$type),
  
  Summary.table <- rbind(dat_loop, Summary.table) #bind to a cumulative list dataframe
  # print(df_total)
}

Summary.table.om       <- Summary.table[!is.na(Summary.table$filename),] # omit NAs in filename 
Summary.table.final    <- Summary.table.om %>%
  dplyr::rename(frequency = numberFound,
                duration = avgDurMinutes,
                magnitude = avgAvgpH) %>% 
  dplyr::mutate(EQ = (as.numeric(frequency) * as.numeric(duration)) / as.numeric(magnitude) ) %>% 
  mutate_all(funs(ifelse(is.na(.), 0, .)))  %>% # convert remaining NAs to 0
  dplyr::mutate(Site = gsub(".*_","",filename),
                Date = gsub("_.*","",filename))
Summary.table.final  

library(ggplot2)
Duration_plot <- Summary.table.final %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(duration)/60, fill = as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("Duration of acidifcation events") +
  ylab("Duration of events (hours; minimum 1 hour)")# +
  # facet_wrap(~Date)
# Duration_plot

Frequency_plot <- Summary.table.final %>% 
  dplyr::filter(frequency >0) %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(frequency), fill = as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("Frequency of acidifcation events")  +
  ylab("Number of events") # +
  # facet_wrap(~Date)
# Frequency_plot

EQ_plot <- Summary.table.final %>% 
  dplyr::filter(!EQ == 'NaN') %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(EQ), fill = as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("(F*D)/M of acidifcation events") +
  ylab("[Freq*Duration/Mean_magnitude]") #+
  # facet_wrap(~Date)
# EQ_plot

library(ggpubr)
pdf("C:/Users/samuel.gurr/Documents/Github_repositories/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/Acidification_events/2023_Summary_plots.pdf", 
    height = 12, width =10)
ggarrange(Duration_plot,
          Frequency_plot,
          EQ_plot,
          nrow=3)
dev.off()





# Low Salinity PLOTS ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::





path.p              <- "C:/Users/samuel.gurr/Documents/Github_repositories/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/LowSalinity_events" #the location of all your respirometry files 
file.names.table    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',sep=''), pattern = "Summary.csv$", recursive = FALSE)))) 


Summary.table            <- data.frame(matrix(nrow = 1, ncol = 6)) # create dataframe to save cumunalitively during for loop
colnames(Summary.table)  <- c('filename',
                              'type',
                              'threshold',
                              'numberFound',
                              'avgDurMinutes',
                              'avgAvgSalinity') # names in the for loop

library(dplyr)
for (m in 1:nrow(file.names.table)) {
  dat_loop              <- as.data.frame(read.csv(paste(path.p,'/',file.names.table[m,1], sep=''))) %>%  #reads in the data files
    dplyr::mutate(filename = gsub("_Sonde.*","", file.names.table[m,1]))
  #summary_dat              <- summary_dat[!is.na(summary_dat$type),
  
  Summary.table <- rbind(dat_loop, Summary.table) #bind to a cumulative list dataframe
  # print(df_total)
}

Summary.table.om       <- Summary.table[!is.na(Summary.table$filename),] # omit NAs in filename 
Summary.table.final    <- Summary.table.om %>%
  dplyr::rename(frequency = numberFound,
                duration = avgDurMinutes,
                magnitude = avgAvgSalinity) %>% 
  dplyr::mutate(EQ = (as.numeric(frequency) * as.numeric(duration)) / as.numeric(magnitude) ) %>% 
  mutate_all(funs(ifelse(is.na(.), 0, .)))  %>% # convert remaining NAs to 0
  dplyr::mutate(Site = gsub(".*_","",filename),
                Date = gsub("_.*","",filename))
Summary.table.final  

library(ggplot2)
Duration_plot <- Summary.table.final %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(duration)/60, fill = as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("Duration of low salinity  events") +
  ylab("Duration of events (hours; minimum 1 hour)") #+
  # facet_wrap(~Date)
# Duration_plot

Frequency_plot <- Summary.table.final %>% 
  dplyr::filter(frequency >0) %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(frequency), fill = as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("Frequency of low salinity  events") +
  ylab("Number of events") #+
  # facet_wrap(~Date)
# Frequency_plot

EQ_plot <- Summary.table.final %>% 
  dplyr::filter(!EQ == 'NaN') %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(EQ), fill = as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("(F*D)/M of low salinity events") +
  ylab("[Freq*Duration/Mean_magnitude]") #+
  # facet_wrap(~Date)
# EQ_plot

library(ggpubr)
pdf("C:/Users/samuel.gurr/Documents/Github_repositories/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/LowSalinity_events/2023_Summary_plots.pdf", 
    height = 12, width =10)
ggarrange(Duration_plot,
          Frequency_plot,
          EQ_plot,
          nrow=3)
dev.off()