# Track plotting
library(ggplot2)
library(mapdata)
library(dplyr)
library(marmap)
library(oceanmap)
library(stringr)
data("cmap")

#--------------------------------------------
# MODIFY THESE MANUALLY FOR EACH TAG
#--------------------------------------------
# Specify tag number
tagnum <- 168219
# This specifies the start and end locations
# Manually entered from the metadata file
se <- data.frame(Lon=c(134.1452 ,133.8229), Lat=c(7.2615, 7.2429))
#--------------------------------------------
#--------------------------------------------

# Set working directory
setwd(paste0('C:\\Users\\Nerea\\Desktop\\NOAA\\PROJECTS & COLLABORATIONS\\PROJECTS\\HAWAII\\YFT\\ALEX\\', tagnum))

# Read data
# This lists all the GPE3 files in the folder so you can read the in at once
gpeFiles <- list.files(pattern='GPE3.csv')

# This part goes through all the GPE3 files you have and loads them into one file
track <- data.frame()
for (i in seq_along(gpeFiles)) {
  # Read in the GPE file skipping the header
  a <- read.csv(gpeFiles[i], skip=5)
  # Grab the speed from the header of the GPE file. This assumes that all GPE headers are the same. Worked for mine and one of Melanies tags
  # Read in the second line of the .csv GPE file header (it's the one that has the speed information in it)
  h <- readLines(gpeFiles[i], n=2)[2]
  # Grab the first number in the character string
  speeds <- as.numeric(str_extract(h, "\\-*\\d+\\.*\\d*"))
  track <- rbind(track, cbind(a, speed=speeds))
}
names(track)[4:5] <- c('Lat','Lon')
head(track)

# Calculate map limits
# Making the limits 5 degrees outside the track
# If your track is short you might want to change this to something smaller
lonMax <- max(track$Lon)+5
lonMin <- min(track$Lon)-5
latMax <- max(track$Lat)+5
latMin <- min(track$Lat)-5

# Get bathymetry
hi <- getNOAA.bathy(lonMin, lonMax, latMin, latMax, resolution=2)

# Plot using ggplot
autoplot(hi, geom=c("raster", "contour"), colour='#141740', size=0.1) + 
  scale_fill_gradientn(colors=rev(cmap$dark.blue), limits=c(min(hi),0)) +
  geom_path(data=track, aes(Lon, Lat, color=as.factor(speed), group=speed), size=0.4) +
  geom_point(data=se, aes(Lon, Lat), shape=c(16,17), color='lemonchiffon') +
  scale_color_viridis_d(direction=-1, option='magma') +
  labs(color='Swim Speed', fill='Depth') +
  xlab('Longitude') +
  ylab('Latitude') +
  ggtitle(paste('Tag', tagnum, 'track at different speeds'))

# Save the plot
outfile <- paste('Tag', tagnum, 'Track.png', sep='_')
ggsave(outfile, dpi=300, width=6, height=11, units='in')
