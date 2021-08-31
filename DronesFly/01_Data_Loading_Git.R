
# Libraries loading -------------------------------------------------------
library(readxl)
library(tidyverse)
library(plotly)

# Plot real data from excel -----------------------------------------------
# CYLINDRICAL coordinates assuming Rho = radius

Angles_Mav_ <- read_excel("data/Compared_Angles_Mav_2018-02-21.xlsx",
                          col_types = c("skip", "skip", "skip", 
                                        "skip", "skip", "numeric", "numeric", 
                                        "numeric", "skip", "skip", "skip", 
                                        "skip", "skip", "skip", "skip"))

Angles_Mav_ <- na.exclude(Angles_Mav_)


# Convert  data from polar CYLINDRICAL to cartesian coordinates ----------------------------------
# assuming Rho = radius(r) and z

rm(x,y,z)
x <- vector(mode="numeric", length=0)
y <- vector(mode="numeric", length=0)
z <- vector(mode="numeric", length=0)
for (i in 1:nrow(Angles_Mav_)) {
  
  radius <- as.numeric(Angles_Mav_$`Distance (Rho)`[i]) #distance rho
  theta <- as.numeric(Angles_Mav_$`Angles (Theta)`[i]) # angle theta
  altitude <- Angles_Mav_$`Altitude (z)`[i] #z
  
  x[i] <-  radius * cos(theta)
  y[i] <-  radius * sin(theta)
  z[i] <-  altitude
  
}

#bind  columns 
Angles_Mav_xyz <-  bind_cols(nro = 1:nrow(Angles_Mav_), x = x,y = y,z = z)



# 3D Plot -----------------------------------------------------------------

plot_ly(Angles_Mav_xyz, y= ~y,  x= ~x, z = ~z , showscale=F) #colors= c('#BF382A', '#0C4B8E', '#ff9900')

