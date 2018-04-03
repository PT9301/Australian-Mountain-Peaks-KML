install.packages("rgdal")
library(rgdal)

qldPeaksRaw <- readOGR("C:/Users/New/Desktop/Qld_Peaks_Capes.kml", "kml_layer", stringsAsFactors = F)
mtVec <- c()
capeVec <- c()
eVec <- c()
hVec <- c()
nameVec <- c()
latVec <- c()
longVec <- c()
qldPeaksDetails <- qldPeaksRaw$Description


for (i in 1:length(qldPeaksDetails))
{
  if (any(grep("<td>FEATURETYPE</td> <td>Mountain</td>", qldPeaksDetails[i])) == TRUE)
  {
    mtVec <- c(mtVec, i)
  }
  else {
    capeVec <- c(capeVec, i)
  }
}

for (j in mtVec)
{
  thise1 <- strsplit(qldPeaksDetails[j], "<td>TEXTNOTE")[[1]]
  thise1 <- strsplit(thise1[2], "</td>")[[1]]
  thise1 <- sub("<td>", "", thise1[2])
  thisElevation <- sub(" +?m", "", sub(" +?", "", thise1))
  eVec <- c(eVec, as.integer(thisElevation))
  
  thisn1 <- strsplit(qldPeaksDetails[j], "<td>NAME")[[1]]
  thisn1 <- strsplit(thisn1[2], "</td>")[[1]]
  thisn1 <- sub(" <td>", "", thisn1[2])
  
  if (any(grep("&lt;Null&gt;", thisn1)) == TRUE)
  {
    thisn1 = "unnamed peak"
  }
  nameVec <- c(nameVec, thisn1)
  
  thislat1 <- qldPeaksRaw@coords[j, 2]
  latVec <- c(latVec, thislat1)
  
  thislon1 <- qldPeaksRaw@coords[j, 1]
  longVec <- c(longVec, thislon1)
}

for (k in 1:length(eVec))
{
  if (is.na(eVec[k]))
  {
    hVec <- c(hVec, NA)
  }
  
  else if (eVec[k] < 500)
  {
    hVec <- c(hVec, 1)
  }
  else if (eVec[k] >= 500 & eVec[k] <= 1000)
  {
    hVec <- c(hVec, 2)
  }
  else if (eVec[k] > 1000)
  {
    hVec <- c(hVec, 3)
  }
  else {
    hVec <- c(hVec, NA)
  }
}

qldPeaks <- data.frame(ElevationMetres=eVec, Name=nameVec, Latitude=latVec, Longitude=longVec, HeightClass=hVec, stringsAsFactors = F)
View(qldPeaks)

boxplot(qldPeaks$ElevationMetres, horizontal = T, xlab="Mountain Height (m)", main="Queensland Mountain Peak Heights")
# Noted error in dataset and on QTopo page, where height of Barra Castle Hill on Hinchinbrook Island recorded as 1910m.

plot(qldPeaks$Longitude, qldPeaks$Latitude, asp=1, xlab="Longitude", ylab="Latitude", main="Queensland Peaks by Lat/Long", col=qldPeaks$HeightClass)
legend("topright", legend=c("<500m", "500-1000m", ">1000m"), pch=15, col=unique(na.omit(qldPeaks$HeightClass)), bty="n")

sum(is.na(qldPeaks$ElevationMetres))