install.packages("dplyr")

kmlPeaks.text <- readLines("C:/Users/New/Desktop/Australia Peaks.kml")
# View(kmlPeaks.text)

rePlacemark <- "<Placemark>"            # Get line positions of placemark tags (total number is total number of data records)
item <- grep(rePlacemark, kmlPeaks.text)

reName <- "<name>"                  # Get the line numbers which correspond to all name elements in the .kml file
namePos <- grep(reName, kmlPeaks.text)

reLong <- "<longitude>"             # Get the line numbers which correspond to all longitude elements in the .kml file
longs <- grep(reLong, kmlPeaks.text)

reFold1 <- "^\t\t<Folder>"        # Get the line numbers which correspond to the start of each state's folder of peaks
stateFolders <- grep(reFold1, kmlPeaks.text)

reFold2 <- "^\t\t\t<Folder>"       # Get the line numbers which correspond to the start of each region's folder of peaks (within a state)
regionFolders <- grep(reFold2, kmlPeaks.text)

nameVec <- c()
heightVec <- c()
hAccVec <- c()
stateVec <- c()
longVec <- c()
latVec <- c()
iVec <- c()       # Provides list of rows where names recorded for debugging purposes
# nameType <- c()   - used to check if names in nameVec are for Folders or Placemarks.
for (i in namePos)
{
  if (any(grep("<Folder>|<Document>", kmlPeaks.text[(i-1)])) == TRUE) # If the name is for a folder, remove from list
  {
    next
  }
  newName <- sub("\t*<name>", "", kmlPeaks.text[i])    # Remove first name tag from name
  newName <- sub("</name>", "", newName)
  newName <- sub("&apos;", "\\'", newName)             # Replace representation of apostrophes in names to actual apostrophes
  # newName <- strsplit(newName, "\(")[[1]][1]
  # nameFile <- sub("<name>", "", kmlPeaks.text[(i-1)])
  # nameType <- c(nameType, nameFile)   - used to check if names in nameVec are for Folders or Placemarks.
  
  heightVal <- sub("( +?)\\(", "^", newName)
  heightVal <- substr(heightVal, 0, nchar(heightVal)-1)   # Remove end bracket, that was paried with the bracket removed in the above line
  heightVal <- strsplit(heightVal, "\\^")[[1]][2]
  
  hAccVal <- any(grep("(c)", heightVal))          # Create logical variable that stores whether height was estimated from map or actual height retrieved
  hAccVec <- c(hAccVec, hAccVal)
  
  heightVal <- sub("m \\(c\\)|m", "", heightVal)    # Remove units and estimated symbol from height, so it is now an integer
  heightVec <- c(heightVec, as.integer(heightVal))
  
  newName <- sub(" \\(.*\\)", "", newName)       # Remove height info from name
  nameVec <- c(nameVec, newName)
  
  if (i > stateFolders[1] & i < stateFolders[2])
  {
    stateVal = "TAS"
  }
  else if (i > stateFolders[2] & i < stateFolders[3])
  {
    stateVal = "QLD"
  }
  else if (i > stateFolders[3] & i < stateFolders[4])
  {
    stateVal = "ACT"
  }
  else if (i > stateFolders[4] & i < stateFolders[5])
  {
    stateVal = "NSW"
  }
  else if (i > stateFolders[5])
  {
    stateVal = "VIC"
  }
  else
  {
    stateVal = NA
  }
  
  stateVec <- c(stateVec, stateVal)
  
  iVec <- c(iVec, i)          # Provides list of rows where names recorded for debugging purposes
}

for (l in longs)
{
  longVal <- sub("\t*<longitude>", "", kmlPeaks.text[l])
  longVal <- sub("</longitude>", "", longVal)
  longVec <- c(longVec, as.numeric(longVal))
  
  latVal <- sub("\t*<latitude>", "", kmlPeaks.text[(l+1)])
  latVal <- sub("</latitude>", "", latVal)
  latVec <- c(latVec, as.numeric(latVal))
}

# nameVec             - Prints the contents of these vectors (debugging)
# heightVec
# stateVec

Peaks <- data.frame(RowNo=iVec, Name=nameVec, HeightMetres=heightVec, isHeightEstimated=hAccVec, State=as.factor(stateVec), Latitude=latVec, Longitude=longVec, stringsAsFactors = F)

View(Peaks)

nswCount <- sum(Peaks$State == "NSW")
qldCount <- sum(Peaks$State == "QLD")
vicCount <- sum(Peaks$State == "VIC")
actCount <- sum(Peaks$State == "ACT")
tasCount <- sum(Peaks$State == "TAS")

PeakCount <- data.frame(state = c("NSW", "QLD", "VIC", "ACT", "TAS"),
                        number = c(nswCount, qldCount, vicCount, actCount, tasCount)
                        )

plot(x=Peaks$Longitude, xlab="Longitude", y=Peaks$Latitude, ylab="Latitude", asp=1, main="Australian Mountains Plotted by Lat\\Long", col=Peaks$State)
legend("bottomright", legend=unique(Peaks$State), pch=15, col=unique(Peaks$State), bty="n")

boxplot(Peaks$HeightMetres~Peaks$State, horizontal = F, xlab="State", ylab="Height (metres)", main="Australian Peak Heights by State")

barplot(PeakCount$number, main="Peak Count by State", xlab="State", ylab="Peak Count", names.arg=PeakCount$state, col=PeakCount$state)

View(PeakCount)

library(dplyr)

PeaksQLD <- filter(Peaks, Peaks$State=="QLD")
PeaksNSW <- filter(Peaks, Peaks$State=="NSW")
PeaksACT <- filter(Peaks, Peaks$State=="ACT")
PeaksVIC <- filter(Peaks, Peaks$State=="VIC")
PeaksTAS <- filter(Peaks, Peaks$State=="TAS")

View(PeaksQLD)

# To test how names of folders can distinguished from names of placemarks:
#
# ifType = c()
# for (k in nameType)
# {
#   check = any(grep("Folder", k))
#   ifType = c(ifType, check)
# }
# ifType

