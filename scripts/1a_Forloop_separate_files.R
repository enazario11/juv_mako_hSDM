files = list.files("C:/Users/Emily Nazario/Documents/R/Projects/Preliminary Shark SDMs/data/Mako Data/Final locs") #can add argument to only select for file type or character string
files

setwd("C:/Users/Emily Nazario/Documents/R/Projects/Preliminary Shark SDMs/data/Mako Data/Final locs")

#set loop to combine all individual files into one DF
shark.locs = NULL

for (i in 1:length(unique(files))) {
  id = read.csv(files[[i]]) %>%
    as_tibble()
  #add shark id as a column
  id = id %>%
    filter(!is.na(Time)) %>%
    mutate(ptt = substr(files[[i]], 1, 8),
           date_time = as.character(paste0(Date, " ", Time)))
  #changes date/time to posix format so R can better interpret
  id$posix = as.POSIXct(strptime(id$date_time, 
                                  format = "%m/%d/%Y %H:%M:%S"))
  #add additional commands to change change, add mo/day, rename columns, etc. HERE
  shark.locs = rbind(shark.locs, id) #adds extra rows at bottom to combine all shark files
}

table(shark.locs$ptt) #prints summary of final contents of combined shark DF


shark.locs %>% #summarizes the durations of the deployments/animal
  group_by(ptt) %>%
  summarise(start= first(posix),
            end = last(posix), 
            n = n()) %>% #provides the number of locations for each shark 
  mutate(dur = difftime(end, start, units = "days")) #can add as.numeric() before difftime to remove the word days from column

