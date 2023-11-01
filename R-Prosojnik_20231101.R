################################################################################
#
# R-Prosojnik ver 1.01
#
# Risanje trajektorij sonca za leta 2020, 2021 in 2022
# glede na Prednje okno v Prisanku
# in najboljsi izracun datuma za opazovanje sonca skozi okno
# Program lahko s spremembami zacetnih nastavitev prilagodimo za:
# poljubno razpoko na obzorju in
# poljubno stojisce fotografa
# ter poljuben datum
#
# Drawing sun trajectories of the Sun for years 2020, 2021 and 2020
# when it was shining through a gap in the mountain Prisank
# The program can be modified by changing the constants for
# any position of the gap, any position of a photographer
# and any selected date.
# 
# Matjaz Jeran
# 01.11.2023
# Tested on R ver 4.3.1 and suncalc ver 0.5.1
#
# This is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details <http://www.gnu.org/licenses/>.

################################################################################

# setting environment

# origin of linux dates
Origin <- "1970-01-01"

# Geographical position of photographer
#Lat <- 46.035
#Lon <- 14.52917
Lat <- 46.43928
Lon <- 13.75629


# Sun position when shining through the gap was proven
ref.azi <- 172.82
ref.alt <- 26.66

# Number of sun points calculated
no.points <- 95

# Criteria when observation is possible
Near <- 0.9

# Limits for the plot of Sun trajectories
min.azi <- 165
max.azi <- 185

# Selected dates when the Sun could be observed through the gap
tdates <- c (
#"2012-11-06 10:19:00", "2012-11-07 10:19:00", "2012-11-08 10:19:00", "2012-11-09 10:19:00",
#"2012-11-10 10:19:00", "2012-11-11 10:19:00", "2012-11-12 10:19:00", "2012-11-13 10:19:00")
#"2020-11-06 10:19:00", "2020-11-07 10:19:00", "2020-11-08 10:19:00", "2020-11-09 10:19:00",
#"2020-11-10 10:19:00", "2020-11-11 10:19:00", "2020-11-12 10:19:00", "2020-11-13 10:19:00")
#"2021-11-06 10:19:00", "2021-11-07 10:19:00", "2021-11-08 10:19:00", "2021-11-09 10:19:00",
#"2021-11-10 10:19:00", "2021-11-11 10:19:00", "2021-11-12 10:19:00", "2021-11-13 10:19:00")
#"2022-01-29 10:50:00", "2022-01-30 10:50:00", "2022-01-31 10:50:00", "2022-02-01 10:50:00",
#"2022-02-02 10:50:00", "2022-02-03 10:50:00", "2022-02-04 10:50:00", "2022-02-05 10:50:00")
#"2022-11-06 10:19:00", "2022-11-07 10:19:00", "2022-11-08 10:19:00", "2022-11-09 10:19:00",
#"2022-11-10 10:19:00", "2022-11-11 10:19:00", "2022-11-12 10:19:00", "2022-11-13 10:19:00")
#"2023-01-29 10:50:00", "2023-01-30 10:50:00", "2023-01-31 10:50:00", "2023-02-01 10:50:00",
#"2023-02-02 10:50:00", "2023-02-03 10:50:00", "2023-02-04 10:50:00", "2023-02-05 10:50:00")
"2023-11-06 10:19:00", "2023-11-07 10:19:00", "2023-11-08 10:19:00", "2023-11-09 10:19:00",
"2023-11-10 10:19:00", "2023-11-11 10:19:00", "2023-11-12 10:19:00", "2023-11-13 10:19:00")
#"2024-01-31 10:50:00", "2024-02-01 10:50:00", "2024-02-02 10:50:00", "2024-02-03 10:50:00",
#"2024-02-04 10:50:00", "2024-02-05 10:50:00", "2024-02-06 10:50:00", "2024-02-07 10:50:00")
#"2024-11-04 10:19:00", "2024-11-05 10:19:00", "2024-11-06 10:19:00", "2024-11-07 10:19:00",
#"2024-11-08 10:19:00", "2024-11-09 10:19:00", "2024-11-10 10:19:00", "2024-11-11 10:19:00")
#"2025-01-30 10:50:00", "2025-01-31 10:50:00", "2025-02-01 10:50:00", "2025-02-02 10:50:00",
#"2025-02-03 10:50:00", "2025-02-04 10:50:00", "2025-02-05 10:50:00", "2025-02-06 10:50:00")
#"2025-11-05 10:19:00", "2025-11-06 10:19:00", "2025-11-07 10:19:00", "2025-11-08 10:19:00",
#"2025-11-09 10:19:00", "2025-11-10 10:19:00", "2025-11-11 10:19:00", "2025-11-12 10:19:00")
  

# Program messages
# Slovenian messages
txt.main <- "Poti sonca blizu okna po dnevih"
txt.xlab <- "Azimut (stopinje)"
txt.ylab <- "Višina (stopinje)"
txt.opttime <- "Najboljši datum in čas opazovanja:"
txt.loc <- "Lokacija:"
txt.gap <- "Okno:"

# English messages
# txt.main <- "Trajectories of the Sun near the gap by days"
# txt.xlab <- "Azimuth (degrees)"
# txt.ylab <- "Altitude (degrees)"
# txt.opttime <- "The best date and time for observation:"
# txt.loc <- "Location"
# txt.gap <- "Gap:"

###############################
# start of calculations
tdates <- strptime (tdates,format = "%Y-%m-%d %H:%M:%S")
xdates <- as.POSIXct (tdates, origin = Origin)
mat.dates <- array (xdates, dim = c (length(xdates), no.points))
rm (xdates)

# generate points in time for every minute (60 seconds)
for (i in 1:length (tdates)) {
 for (j in 2:no.points) mat.dates [i, j] <- mat.dates [i, j-1] + 60
}
rm (tdates)

# function suncalc does not accept arrays, so we calculate Sun positions in a loop
mat.result <- NULL
for (i in 1:dim (mat.dates) [1]) {
 for (j in 1:dim (mat.dates) [2]) {
   result <- suncalc::getSunlightPosition (
     date = as.POSIXct(mat.dates[i,j], origin = Origin),
     lat = Lat, lon = Lon, keep = c ("altitude", "azimuth"))
   mat.result <- rbind (mat.result, result)
  }
}
rm (i, j)

# calculate table of dates and times with sun positions in degrees
mat.result.deg <- data.frame (
  date = mat.result$date,
  alt  = mat.result$altitude * 180 / pi,
  azi  = ((mat.result$azimuth + pi) * 180 / pi) %% 360)
rm (mat.dates)

# select moments when the Sun is nearest to the desired point through the gap
# sort mat.result.deg by distance from the desired position of the Sun
# use Manhattan distance: sum of abs differences
dist <- abs (ref.azi - mat.result.deg$azi) + abs (ref.alt - mat.result.deg$alt)
ord.dist <- order (dist)

# print times and positions of the Sun near the reference position
tmp <- subset (mat.result.deg, subset = dist < Near)
print (cbind (tmp, dist = dist [dist < Near]))

# determine time with minimum distance
opt.time <- format (subset (mat.result.deg, subset = dist == min (dist), select = date),
                    format = "%d.%m.%Y %H:%M")


# plot the Sun trajectories
plot (alt ~ azi, data = mat.result.deg,
      xlim = c (min.azi, max.azi), cex = 0.25, col = "orange",
      main = txt.main, xlab = txt.xlab, ylab = txt.ylab)
points (ref.alt ~ ref.azi, cex = 2)

# calculate labels in the graph
tmp.correction <- 0.03
days <- unique (strptime (format (mat.result.deg$date, format = "%Y-%m-%d"), format= "%Y-%m-%d"))
x.lab <- tapply (X = mat.result.deg$azi, INDEX = format (mat.result.deg$date, "%Y-%m-%d"), FUN = max)
y.lab <- tapply (X = mat.result.deg$alt, INDEX = format (mat.result.deg$date, "%Y-%m-%d"), FUN = max) - tmp.correction
text (x = x.lab, y = y.lab, labels = format (days, "%d.%m.%Y"), pos = 4)

# plot the Sun points near ref position in bold
points (alt ~ azi, data = tmp, cex = 0.8, col = "red")

# add comment for best date and time of observation
text (x = 168, y = 23.7, labels = paste (txt.opttime, opt.time), pos = 4)
text (x = 168, y = 23.4, labels = paste (txt.loc, Lat, Lon), pos = 4)
text (x = 175.7, y = 23.4, labels = paste (txt.gap, ref.azi, ref.alt), pos = 4)