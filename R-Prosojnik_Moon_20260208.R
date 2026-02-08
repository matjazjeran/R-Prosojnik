################################################################################
#
# R-Prosojnik_Moon ver 0.9
#
# Risanje trajektorij lune za leto 2026
# glede na Prednje okno v Prisanku
# in najboljsi izracun datuma za opazovanje lune skozi okno
# Program lahko s spremembami zacetnih nastavitev prilagodimo za:
# poljubno razpoko na obzorju in
# poljubno stojisce fotografa
# ter poljuben datum
#
# Drawing moon trajectories for year 2026
# when it was shining through a gap in the mountain Prisank
# The program can be modified by changing the constants for
# any position of the gap, any position of a photographer
# and any selected date.
# 
# Matjaz Jeran
# 08.02.2026
# Tested on R ver 4.5.2 and suncalc ver 0.5.1
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

rm (list = ls (all = TRUE))

getwd ()

# Define years of calculation
Current.year <- "2026"
Next.year <- "2027"

# Geographical position of photographer
# Pod Tonkino koco na Vrsicu
Lat <- 46.43928
Lon <- 13.75629

# Pod nekdanjim Hotelom Erika pod Vrsicem ???
#Lat <- 46.035
#Lon <- 14.52917

# Sun position when shining through the gap was proven
# Pod Tonkino koco na Vrsicu
ref.azi <- 172.82
ref.alt <- 26.66

# Pod nekdanjim Hotelom Erika pod Vrsicem ???
#ref.azi <- 186
#ref.alt <- 15.5

# Criteria when observation is possible
Near <- 0.9


# Limits for the plot of trajectories - may be modified as necessary
min.azi <- 165
max.azi <- 185

min.alt <- 22
max.alt <- 29


# Program messages
# Slovenian messages
txt.main <- "Poti lune blizu okna po dnevih"
txt.azi <- "Azimut"
txt.xlab <- "Azimut (stopinje)"
txt.alt <- "Višina"
txt.ylab <- "Višina (stopinje)"
txt.opttime <- "Najboljši datum in čas opazovanja:"
txt.loc <- "Lokacija (lat lon):"
txt.gap <- "Okno (az viš):"

# English messages
# txt.main <- "Trajectories of the Moon near the gap by days"
# txt.xlab <- "Azimuth (degrees)"
# txt.ylab <- "Altitude (degrees)"
# txt.opttime <- "The best date and time for observation:"
# txt.loc <- "Location"
# txt.gap <- "Gap:"


library (suncalc)

###############################
# start of calculations
tdates <- as.POSIXct (c (paste0 (Current.year, "-01-01 00:00:00"), paste0 (Next.year, "-01-01 00:00:00")), format = "%Y-%m-%d %H:%M:%S")
xdates <- seq (from = tdates [1], to = tdates[2], by = "1 min")

dates <- data.frame (date = xdates, lat = Lat, lon = Lon)

# for moon
mat.result0 <- suncalc::getMoonPosition (
  data = dates,
  keep = c ("altitude", "azimuth", "distance", "parallacticAngle"))

mat.result1 <- suncalc::getMoonIllumination (
  date = xdates,
  keep = c ("fraction", "phase", "angle"))

mat.result <- merge (mat.result0, mat.result1)
            
# calculate table of dates and times with sun positions in degrees
result <- data.frame (
  date = mat.result$date,
  alt  = mat.result$altitude * 180 / pi,
  azi  = ((mat.result$azimuth + pi) * 180 / pi) %% 360,
  distance = mat.result$distance,
  parallacticAngle = mat.result$parallacticAngle,
  fraction = mat.result$fraction,
  phase = mat.result$phase,
  angle = mat.result$angle)
# rm (mat.dates)

# select moments when the Sun is nearest to the desired point through the gap
# sort mat.result.deg by distance from the desired position of the Sun
# use Manhattan distance: sum of abs differences
dist <- abs (ref.azi - result$azi) + abs (ref.alt - result$alt)
ord.dist <- dist [order (dist)]

# print times and positions of the Sun near the reference position
opt.set <- subset (result, subset = dist < Near)
print (cbind (opt.set, dist = dist [dist < Near]))


# there are more than one set of optimal results
# making graphs is not finished yet

# determine time with minimum distance
opt.time <- subset (opt.set, subset = dist == min (dist), select = date)
f.opt.time <- round (opt.time$date[1], "day")
lim.times <- c (f.opt.time - 24*4*3600, f.opt.time + 24*4*3600)

# plot all relevant trajectories
plot (alt ~ azi, data = subset (result, subset = lim.times [1] < date & date < lim.times [2]),
      xlim = c (min.azi, max.azi), ylim = c (min.alt, max.alt),
      cex = 0.25, col = "orange",
      main = txt.main, xlab = txt.xlab, ylab = txt.ylab)
points (ref.alt ~ ref.azi, cex = 2)


# calculate labels in the graph
tmp.correction <- 0.03
days <- unique (strptime (format (opt.set$date, format = "%Y-%m-%d"), format= "%Y-%m-%d"))
x.lab <- tapply (X = opt.set$azi, INDEX = format (opt.set$date, "%Y-%m-%d"), FUN = max)
y.lab <- tapply (X = opt.set$alt, INDEX = format (opt.set$date, "%Y-%m-%d"), FUN = max) - tmp.correction
text (x = x.lab, y = y.lab, labels = format (days, "%d.%m.%Y"), pos = 4)

# plot the moon points near ref position in bold
points (alt ~ azi, data = opt.set, cex = 1, col = "red")

# add comment for best date and time of observation
text (x = 168, y = 23.7, labels = paste (txt.opttime, format (opt.time, "%Y-%m-%d %H:%M:%S")), pos = 4)
text (x = 168, y = 23.4, labels = paste (txt.loc, Lat, Lon), pos = 4)
text (x = 175.7, y = 23.4, labels = paste (txt.gap, ref.azi, ref.alt), pos = 4)

print (paste (txt.opttime, format (opt.time, "%Y-%m-%d %H:%M:%S")))
print (paste (txt.loc, Lat, Lon))
print (paste (txt.gap, txt.azi, ref.azi, "  ", txt.alt, ref.alt))