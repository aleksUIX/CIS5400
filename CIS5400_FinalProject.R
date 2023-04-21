#Charlie King and Alex Sekowski
#CIS 5400 Final Project Code


#importing dataset ############
library(readxl)
library(stringr)

UCS_Satellite_Database_5_1_2022 <- read_xls("C:/Aleks/FIT/Spring 2023/CIS5400 Data Analysis/project/db.xls") #replace with file path on your computer
#view(UCS_Satellite_Database_5_1_2022)




ggplot(UCS_Satellite_Database_5_1_2022, aes(x=UCS_Satellite_Database_5_1_2022$`Country/Org of UN Registry`, y=UCS_Satellite_Database_5_1_2022$Purpose)) + 
  geom_point(size = 2, color = "blue") + geom_count(col="blue", show.legend=F) +
  labs(x = "X-axis label", y = "Y-axis label", title = "Title of plot") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


ggplot(UCS_Satellite_Database_5_1_2022, aes(x=UCS_Satellite_Database_5_1_2022$`Type of Orbit`, y=UCS_Satellite_Database_5_1_2022$Purpose)) + 
  geom_point(size = 2, color = "blue") +
  labs(x = "X-axis label", y = "Y-axis label", title = "Title of plot") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


ggplot(UCS_Satellite_Database_5_1_2022, aes(x=UCS_Satellite_Database_5_1_2022$`Apogee (km)`, y=UCS_Satellite_Database_5_1_2022$`Inclination (degrees)`)) + 
  geom_point(size = 2, color = "red") +
  labs(x = "X-axis label", y = "Y-axis label", title = "Title of plot") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))



library(ggplot2)
theme_set(theme_classic())

db <- UCS_Satellite_Database_5_1_2022

# plot
g <- ggplot(db, aes(UCS_Satellite_Database_5_1_2022$`Country/Org of UN Registry`, UCS_Satellite_Database_5_1_2022$Purpose))
g + geom_violin() + 
  labs(title="Violin plot", 
       subtitle="City Mileage vs Class of vehicle",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")


ggplot(UCS_Satellite_Database_5_1_2022, aes(x = x, y = y, fill = UCS_Satellite_Database_5_1_2022$)) + 
        geom_tile(color = "black", size = 0.5) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
        scale_fill_brewer(palette = "Set3") +
        labs(title="Waffle Chart", subtitle="'Class' of vehicles",
             caption="Source: mpg") + 
        theme(panel.border = element_rect(size = 2),
              plot.title = element_text(size = rel(1.2)),
              axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              legend.title = element_blank(),
              legend.position = "right")




library(ggplot2)

# Create sample data
df <- data.frame(
  date = seq(as.Date("2001-01-01"), as.Date("2021-01-31"), "day"),
  count = cumsum(UCS_Satellite_Database_5_1_2022)
)

# Create plot
ggplot(df, aes(x = date, y = cumsum(count))) +
  geom_line() +
  stat_summary(fun.y = "last", geom = "point", size = 3, color = "red") +
  xlab("Date") +
  ylab("Cumulative Count") +
  ggtitle("Cumulative Count by Date")





#required packages
install.packages("rgl", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("png", dependencies = TRUE)
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

#axis lengths
x <- rnorm(10000)
y <- rnorm(10000)
z <- rnorm(10000)
aspect3d(1, 1, 1)

# Plot the orbit
plot3d(positions, type = "l", col = "red", xlab = "X (km)", ylab = "Y (km)", zlab = "Z (km)")

#green earth
rgl.spheres(0, 0, 0, radius = re, color = "green", alpha = 0.7)


####################################################################
####################################################################
#plotting multiple orbits and earth - does not work



inputs_list <- list(
  list(
    lon_geo = 0,
    perigee = 6700,
    apogee = 6700,
    eccentricity = 0,
    inclination = 28.5,
    period = 1440
  ),
  list(
    lon_geo = 30,
    perigee = 6700,
    apogee = 7000,
    eccentricity = 0.05,
    inclination = 45,
    period = 90
  )
)

# Loop through input values and plot the orbit for each set of inputs
for (i in seq_along(inputs_list)) {
  # Extract input values
  input_values <- inputs_list[[i]]
  lon_geo <- input_values$lon_geo
  perigee <- input_values$perigee
  apogee <- input_values$apogee
  eccentricity <- input_values$eccentricity
  inclination <- input_values$inclination
  period <- input_values$period
  
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
    R1 <- matrix(c(cos(theta), -sin(theta), 0, sin(theta), cos(theta), 0, 0, 0, 1), nrow=3, byrow=TRUE)
    R3 <- matrix(c(cos(oe["Omega"]), sin(oe["Omega"]), 0, -sin(oe["Omega"]), cos(oe["Omega"]), 0, 0, 0, 1), nrow = 3)
    r <- R1 %*% R3 %*% r
    return(r)
  }
  
  # Calculate position vectors at all times
  r <- apply(matrix(t, ncol = length(t)), 1, r_at_t)
  
  # Plot the orbit
  lines3d(r[,1], r[,2], r[,3], col = i)
}


rgl.spheres(x = 0, y = 0, z = 0, radius = re, color = "green", alpha = 0.5)
