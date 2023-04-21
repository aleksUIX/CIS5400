#Charlie King and Alex Sekowski
#CIS 5400 Final Project Code


#importing dataset ############
library(readxl)
UCS_Satellite_Database_5_1_2022 <- read.csv("db.csv") #replace with file path on your computer
#view(UCS_Satellite_Database_5_1_2022)

#required packages
install.packages("rgl", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("rglwidget", dependencies = TRUE)

library(rgl)
library(ggplot2)
library(png)
library(rglwidget)
#############################################################
#############################################################
#single orbit code with earth - long of geo is not calculated correctly

# Define inputs
lon_geo <- 20 # Longitude of GEO in degrees
perigee <- 5000 # Perigee in km
apogee <- 5000 # Apogee in km
eccentricity <- 0 # Eccentricity
inclination <- 25 # Inclination in degrees
period <- 1000 # Period in minutes

# Define constants
mu <- 3.986e5 # Earth's gravitational parameter (km^3/s^2)
re <- 6378 # Earth's radius in km

# Calculate semimajor axis, semiminor axis, and angular momentum
a <- (perigee + apogee) / 2 + re
b <- a * sqrt(1 - eccentricity^2)
h <- sqrt(mu * a * (1 - eccentricity^2))

# Define the orbital elements
oe <- c(a = a, b = b, h = h, e = eccentricity, i = inclination, Omega = lon_geo, omega = 0)

# Define time vector
t <- seq(0, period * 60, by = 60)

# Define function to calculate position vector at given time
r_at_t <- function(t) {
  E <- 2 * atan(sqrt((1 - eccentricity) / (1 + eccentricity)) * tan(oe["i"]/2) * cos(oe["Omega"] + oe["omega"])) - oe["omega"]
  M <- 2 * pi * t / (period * 60)
  E_new <- M + eccentricity * sin(E)
  while(abs(E - E_new) > 1e-8) {
    E <- E_new
    E_new <- M + eccentricity * sin(E)
  }
  nu <- 2 * atan(sqrt((1 + eccentricity) / (1 - eccentricity)) * tan(E/2))
  r <- oe["h"]^2 / mu * (1 / (1 + eccentricity * cos(nu))) * c(cos(nu), sin(nu), 0)
  R1 <- matrix(c(1, 0, 0, 0, cos(oe["i"]), sin(oe["i"]), 0, -sin(oe["i"]), cos(oe["i"])), nrow = 3)
  R3 <- matrix(c(cos(oe["Omega"]), sin(oe["Omega"]), 0, -sin(oe["Omega"]), cos(oe["Omega"]), 0, 0, 0, 1), nrow = 3)
  r <- R1 %*% R3 %*% r
  return(r)
}

# Calculate position vectors for all time points
positions <- t(sapply(t, r_at_t))
positions

# Plot the orbit
plot3d(positions, type = "l", col = "red", xlab = "X (km)", ylab = "Y (km)", zlab = "Z (km)")

#green earth
rgl.spheres(0, 0, 0, radius = re, color = "green", alpha = 0.7)


####################################################################
####################################################################
#plotting multiple orbits and earth - does not work


#defining orbits selected
orbits <- UCS_Satellite_Database_5_1_2022$NORAD.Number
orbits

ps <-  matrix(c(0,0,0),nrow=0,ncol=3,byrow=TRUE)

for(num in orbits) {
  
  #selecting the row
  row_index <- which(UCS_Satellite_Database_5_1_2022$`NORAD.Number` == num)
  print(row_index)

  #Define inputs
  lon_geo <- as.numeric(UCS_Satellite_Database_5_1_2022$`Longitude.of.GEO..degrees.`[row_index]) 
  perigee <- as.numeric(gsub(",", "", UCS_Satellite_Database_5_1_2022$`Perigee..km.`[row_index]))
  apogee <- as.numeric(gsub(",", "", UCS_Satellite_Database_5_1_2022$`Apogee..km.`[row_index]))
  eccentricity <- as.numeric(UCS_Satellite_Database_5_1_2022$Eccentricity[row_index])
  inclination <- as.numeric(UCS_Satellite_Database_5_1_2022$`Inclination..degrees.`[row_index])
  period <- as.numeric(UCS_Satellite_Database_5_1_2022$`Period..minutes.`[row_index])
  print(lon_geo)
  print(apogee)
  print(eccentricity)
  print(inclination)
  print(period)
  
# Define constants
mu <- 3.986e5 # Earth's gravitational parameter (km^3/s^2)
re <- 6378 # Earth's radius in km

# Calculate semimajor axis, semiminor axis, and angular momentum
a <- (perigee + apogee) / 2 + re
b <- a * sqrt(1 - eccentricity^2)
h <- sqrt(mu * a * (1 - eccentricity^2))


# Define the orbital elements
oe <- c(a = a, b = b, h = h, e = eccentricity, i = inclination, Omega = lon_geo, omega = 0)

# Define time vector
t <- seq(0, period * 60, by = 60)

# Define function to calculate position vector at given time
  r_at_t <- function(t) {
    E <- 2 * atan(sqrt((1 - eccentricity) / (1 + eccentricity)) * tan(oe["i"]/2) * cos(oe["Omega"] + oe["omega"])) - oe["omega"]
    M <- 2 * pi * t / (period * 60)
    E_new <- M + eccentricity * sin(E)
    while(abs(E - E_new) > 1e-8) {
      E <- E_new
      E_new <- M + eccentricity * sin(E)
    }
    nu <- 2 * atan(sqrt((1 + eccentricity) / (1 - eccentricity)) * tan(E/2))
    r <- oe["h"]^2 / mu * (1 / (1 + eccentricity * cos(nu))) * c(cos(nu), sin(nu), 0)
    R1 <- matrix(c(1, 0, 0, 0, cos(oe["i"]), sin(oe["i"]), 0, -sin(oe["i"]), cos(oe["i"])), nrow = 3)
    R3 <- matrix(c(cos(oe["Omega"]), sin(oe["Omega"]), 0, -sin(oe["Omega"]), cos(oe["Omega"]), 0, 0, 0, 1), nrow = 3)
    r <- R1 %*% R3 %*% r
    return(r)
  }
  
  ps_t <- t(sapply(t, r_at_t))
  ps <- rbind(ps, ps_t)
}

# Plot the orbit
plot3d(ps, type = "l", col = "red", xlab = "X (km)", ylab = "Y (km)", zlab = "Z (km)")
# Calculate position vectors for all time points


rgl.spheres(x = 0, y = 0, z = 0, radius = re, color = "green", alpha = 1)

