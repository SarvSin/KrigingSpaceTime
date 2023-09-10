# 505 PART C!!

library(dplyr)
library(magrittr)
library(geoR)

# Reading in the metadata file including elevation and location data
meta_data <- read.csv('/Users/sarveshwarisingh/Desktop/505 stuff/metadata.csv')
meta_data

# Initial data analysis
meta_data
plot(meta_data$Elevation)
# Range of elevation
range(meta_data$Elevation)
# Plotting the monitoring locations according to elevation
map('worldHires', "UK",xlim=c(-11,3), ylim=c(49,60.9), )
points(meta_data[15,2], meta_data[15,3], pch=19,col="chocolate1")  # Lerwick 
points(meta_data[1,2], meta_data[1,3], pch=19,col="chocolate1")  # Machri 
points(meta_data[2,2], meta_data[2,3], pch=19,col="chocolate4")  # High Wycombe
points(meta_data[3,2], meta_data[3,3], pch=19,col="chocolate1")  # Camborne 
points(meta_data[4,2], meta_data[4,3], pch=19,col="chocolate4")  # Dun Fell
points(meta_data[5,2], meta_data[5,3], pch=19,col="chocolate1")   # Plymouth
points(meta_data[6,2], meta_data[6,3], pch=19,col="chocolate1")  # Durham 
points(meta_data[7,2], meta_data[7,3], pch=19,col="chocolate1")   # London
points(meta_data[8,2], meta_data[8,3], pch=19,col="chocolate1")  # Porthmadog 
points(meta_data[9,2], meta_data[9,3], pch=19,col="chocolate1")   # Leconfield
points(meta_data[10,2], meta_data[10,3], pch=19,col="chocolate4") # knross  
points(meta_data[11,2], meta_data[11,3], pch=19,col="chocolate1")  # Morecambe 
points(meta_data[12,2], meta_data[12,3], pch=19,col="chocolate1")  # Lossiemouth 
points(meta_data[13,2], meta_data[13,3], pch=19,col="chocolate1")   # Marham
points(meta_data[14,2], meta_data[14,3], pch=19,col="chocolate1")   # Whitby
points(meta_data[16,2], meta_data[16,3], pch=19,col="chocolate1")   # Yeovilton
points(meta_data[17,2], meta_data[17,3], pch=19,col="chocolate4")   # Sheffield
points(meta_data[18,2], meta_data[18,3], pch=19,col="chocolate4")   # Cove
points(meta_data[19,2], meta_data[19,3], pch=19,col="chocolate1")   # Stornoway
points(meta_data[20,2], meta_data[20,3], pch=19,col="chocolate4")  # Lyneham 
title("MET Office monitoring locations, United Kingdom")


# Reading in the maximum temperature file
temp_data <- read.csv('/Users/sarveshwarisingh/Desktop/505 stuff/MaxTemp.csv')
# Initial data analysis
summary(temp_data)
plot(GGally::ggpairs(temp_data))
class(temp_data$Date)




# August 15 temperature data
aug.15 <- temp_data%>%
  filter(Date == 20200815)

# To - fit a spatial model to predict the maximum temperature in Leconfield, 
# Yeovilton, and High Wycombe on August 15, 2020

# August 15 is the golden day so temperature data for the same will be extracted
select.aug <- aug.15[,-c(1, 3, 10, 17)]

# Selecting temperature data
temp.train <- as.numeric(select.aug[1,])
# Selecting location and elevation data
loc.train <- meta_data[-c(2, 9, 16),]
# Merging the two datasets
loc.train$temp <- temp.train
loc.train
# Converting to geodata object
train.gdat <- as.geodata(loc.train, coords.col=2:3, data.col=5, covar.col = 4)
train.gdat

# Plotting the temperature data
plot(train.gdat)
# There appears to be a spatial correlation in temperature with higher temperatures 
# In the south-west, and lower temperatures in the north and north-west

plot(variog(train.gdat))
# The variogram appears to be very noisy with few data points; it would be wise not
# to fit a model on these points

# Checking for anisotropy
plot(variog4(train.gdat, coords=train.gdat$coords, data=train.gdat$data))

# MLE
# Matern 1.5 - with trend: elevation + 1st
AIC(likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Elevation+Latitude+Longitude, kappa=1.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(train.gdat, model=likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Elevation+Latitude+Longitude, kappa=1.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Matern 1.5 - with trend: elevation + 2nd
AIC(likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Elevation+Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), kappa=1.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(train.gdat, model=likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Elevation+Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), kappa=1.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Matern 1.5 - with trend: 1st
AIC(likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Latitude+Longitude, kappa=1.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(train.gdat, model=likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Latitude+Longitude, kappa=1.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Matern 1.5 - with trend: 2nd
AIC(likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), kappa=1.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(train.gdat, model=likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), kappa=1.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Matern 1.5 - without trend
AIC(likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", kappa=1.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(train.gdat, model=likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", kappa=1.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))


# Now testing some more covariance models - 

# Matern 0.5 - with trend: elevation + 1st
AIC(likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Elevation+Latitude+Longitude, kappa=0.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(train.gdat, model=likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Elevation+Latitude+Longitude, kappa=0.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Matern 0.5 - with trend: elevation + 2nd
AIC(likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Elevation+Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), kappa=0.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(train.gdat, model=likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Elevation+Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), kappa=0.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Matern 0.5 - with trend: 1st
AIC(likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Latitude+Longitude, kappa=0.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(train.gdat, model=likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Latitude+Longitude, kappa=0.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Matern 0.5 - with trend: 2nd
AIC(likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), kappa=0.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(train.gdat, model=likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), kappa=0.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Matern 0.5 - without trend
AIC(likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", kappa=0.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(train.gdat, model=likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", kappa=0.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))



# Squared exponential
# STAR - Squared Exponential - with trend: elevation + 1st
AIC(likfit(train.gdat, ini=c(0.5, 0.5), cov.model="powered.exponential", lik.met="REML",kappa=2, trend=~Elevation+Latitude+Longitude,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(train.gdat, model=likfit(train.gdat, ini=c(0.5, 0.5), lik.met="REML",cov.model="powered.exponential", kappa=2,trend=~Elevation+Latitude+Longitude, fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Squared Exponential - with trend: elevation + 2nd
AIC(likfit(train.gdat, ini=c(0.5, 0.5), cov.model="powered.exponential",kappa=2, trend=~Elevation+Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude),fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(train.gdat, model=likfit(train.gdat, ini=c(0.5, 0.5), cov.model="powered.exponential", kappa=2,trend=~Elevation+Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude),fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Squared Exponential - with trend: 1st
AIC(likfit(train.gdat, ini=c(0.5, 0.5), cov.model="powered.exponential", trend=~Latitude+Longitude, kappa=2,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(train.gdat, model=likfit(train.gdat, ini=c(0.5, 0.5), cov.model="powered.exponential", kappa=2,trend=~Latitude+Longitude, fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Squared Exponential - with trend: 2nd
AIC(likfit(train.gdat, ini=c(0.5, 0.5), cov.model="powered.exponential",kappa=2, trend=~Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(train.gdat, model=likfit(train.gdat, ini=c(0.5, 0.5), cov.model="powered.exponential", kappa=2,trend=~Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Squared Exponential - without trend
AIC(likfit(train.gdat, ini=c(0.5, 0.5), cov.model="powered.exponential", fix.nugget=FALSE, kappa=2,fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(train.gdat, model=likfit(train.gdat, ini=c(0.5, 0.5), cov.model="powered.exponential", kappa=2,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))


# Matern 2.5
# Matern 0.5 - with trend: elevation + 1st
AIC(likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Elevation+Latitude+Longitude, kappa=2.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(train.gdat, model=likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Elevation+Latitude+Longitude, kappa=2.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Matern 0.5 - with trend: elevation + 2nd
AIC(likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Elevation+Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), kappa=2.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(train.gdat, model=likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Elevation+Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), kappa=2.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Matern 0.5 - with trend: 1st
AIC(likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Latitude+Longitude, kappa=2.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(train.gdat, model=likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Latitude+Longitude, kappa=2.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Matern 0.5 - with trend: 2nd
AIC(likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), kappa=2.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(train.gdat, model=likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), kappa=2.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Matern 0.5 - without trend
AIC(likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", kappa=2.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(train.gdat, model=likfit(train.gdat, ini=c(0.5, 0.5), cov.model="matern", kappa=2.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))



# Exponential
# Exponential - with trend: elevation + 1st
AIC(likfit(train.gdat, ini=c(0.5, 0.5), cov.model="exponential", trend=~Elevation+Latitude+Longitude,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(train.gdat, model=likfit(train.gdat, ini=c(0.5, 0.5), cov.model="exponential", trend=~Elevation+Latitude+Longitude, fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Exponential - with trend: elevation + 2nd
AIC(likfit(train.gdat, ini=c(0.5, 0.5), cov.model="exponential", trend=~Elevation+Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude),fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(train.gdat, model=likfit(train.gdat, ini=c(0.5, 0.5), cov.model="exponential", trend=~Elevation+Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude),fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Exponential - with trend: 1st
AIC(likfit(train.gdat, ini=c(0.5, 0.5), cov.model="exponential", trend=~Latitude+Longitude, fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(train.gdat, model=likfit(train.gdat, ini=c(0.5, 0.5), cov.model="exponential", trend=~Latitude+Longitude, fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Exponential - with trend: 2nd
AIC(likfit(train.gdat, ini=c(0.5, 0.5), cov.model="exponential", trend=~Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(train.gdat, model=likfit(train.gdat, ini=c(0.5, 0.5), cov.model="exponential", trend=~Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Exponential - without trend
AIC(likfit(train.gdat, ini=c(0.5, 0.5), cov.model="exponential", fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(train.gdat, model=likfit(train.gdat, ini=c(0.5, 0.5), cov.model="exponential", fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Selecting the model SQUARED EXPONENTIAL WITH TREND INCLUDING ELEVATION+1ST TREND
mod.1 <- likfit(train.gdat, ini=c(0.5, 0.5), lik.met="REML",cov.model="powered.exponential", kappa=2, trend=~Elevation+Latitude+Longitude,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)

# Now finding the mean and variance at a grid of resolution 0.1° over Britain

# First calculating the range of Latitude and Longitude to map over entire Britain
min(meta_data$Longitude)
max(meta_data$Longitude)
min(meta_data$Latitude)
max(meta_data$Latitude)
bri_grid <- expand.grid(seq(-8, 2, by=0.1), seq(48, 62, by=0.1))

# Now finding the mean and variance at points on the grid created above
bri_con <- krige.conv(train.gdat, loc=bri_grid, krige=krige.control(obj.model=mod.1))
library(maps)
# Plotting the means and variances
map('worldHires', "UK",xlim=c(-11,3), ylim=c(49,60.9))
image(bri_con, col = viridis::plasma(25), coords.data = train.gdat[1]$coords, main = 'Mean - ML')
image(bri_con, values = bri_con$krige.var, coords.data = train.gdat[1]$coords,
      main = 'Variance - ML')
pimage(bri_grid[,1],bri_grid[,2],bri_con$predict, col = viridis::plasma(25),  map="world", proj="bonne", parameters=45, points=list(x=as.numeric(train.gdat$coords[,1]),y= as.numeric(train.gdat$coords[,2])), legend = "vertical",  points.args = list(pch = 19, col = "cyan", cex=0.5))
pimage(bri_grid[,1],bri_grid[,2],bri_con$krige.var,  map="world", proj="bonne", parameters=45, points=list(x=as.numeric(train.gdat$coords[,1]),y= as.numeric(train.gdat$coords[,2])), legend = "vertical",  points.args = list(pch = 19, col = "cyan", cex=0.5))


# Now, predicting the maximum daily temperature for Leconfield, Yeovilton, and High Wycombe 
meta_data
# Firstly, creating a dataframe containing the locations
bri_pred <- meta_data[c(2, 9, 16),-c(1,4)]
bri_con.pred <- krige.conv(train.gdat, loc=bri_pred, krige=krige.control(obj.model=mod.1))
bri_con.pred
meta_data[c(2, 9, 16),]
### FITTING A TIME SERIES TO PREDICT MAX. TEMP IN OCT 1ST WEEK ###

library(forecast)

# Selecting temperature data for Lerwick until, and not including October 1, 2020
lerwick.temp <- ts(temp_data[1:274,"Lerwick"])

plot(lerwick.temp,main="Maximum Daily Temperature (Lerwick)",
     xlab="Time (Days since January 1 until October 1, 2020)",
     ylab="Temperature (°C)")
first.dif <- diff(lerwick.temp, 1)
plot(first.dif)
second.diff <- diff(lerwick.temp, 2)
plot(second.diff)

# It is not clear if second order differencing is necessary for removing non-stationarity
# So ARIMA models will be fit to data differenced both once and twice and diagnostics will
# analysed to judge 

# 1st order differencing

# Plotting the ACF and PACF
par(mfrow=c(1,2))
acf(first.dif); pacf(first.dif)

# Given that the PACF slowly tends to zero, while ACF falls quickly to 0 
# A Moving Average model might be most appropriate

# MA(1)
AIC(arima(first.dif, order=c(0,0,1)))
tsdiag(arima(first.dif, order=c(0,0,1)))

# MA(2)
AIC(arima(first.dif, order=c(0,0,2)))
tsdiag(arima(first.dif, order=c(0,0,2)))

# MA(3)
AIC(arima(first.dif, order=c(0,0,3)))
tsdiag(arima(first.dif, order=c(0,0,3)))

# ARMA(1,1)
AIC(arima(first.dif, order=c(1,0,1)))
tsdiag(arima(first.dif, order=c(1,0,1)))

# ARMA(1,2)
AIC(arima(first.dif, order=c(1,0,2)))
tsdiag(arima(first.dif, order=c(1,0,2)))

# ARMA(2,1)
AIC(arima(first.dif, order=c(2,0,1)))
tsdiag(arima(first.dif, order=c(2,0,1)))

# ARMA(2,2)
AIC(arima(first.dif, order=c(2,0,2)))
tsdiag(arima(first.dif, order=c(2,0,2)))

# ARMA(2,3)
AIC(arima(first.dif, order=c(2,0,3)))
tsdiag(arima(first.dif, order=c(2,0,3)))

# ARMA(3,2)
AIC(arima(first.dif, order=c(3,0,2)))
tsdiag(arima(first.dif, order=c(3,0,2)))

# ARMA(3,1)
AIC(arima(first.dif, order=c(3,0,1)))
tsdiag(arima(first.dif, order=c(3,0,1)))

# An ARIMA(2,1,1) model is finalised after checking the results

# 2nd order differencing

# Plotting the ACF and PACF
par(mfrow=c(1,2))
acf(second.diff); pacf(second.diff)

# Both ACF and PACF gradually tend towards 0 thus suggesting the appropriateness of 
# an ARMA model 

# ARMA(1,1)
AIC(arima(second.diff, order=c(1,0,1)))
tsdiag(arima(second.diff, order=c(1,0,1)))

# ARMA(1,2)
AIC(arima(second.diff, order=c(1,0,2)))
tsdiag(arima(second.diff, order=c(1,0,2)))

# ARMA(2,1)
AIC(arima(second.diff, order=c(2,0,1)))
tsdiag(arima(second.diff, order=c(2,0,1)))

# ARMA(2,2)
AIC(arima(second.diff, order=c(2,0,2)))
tsdiag(arima(second.diff, order=c(2,0,2)))

# ARMA(2,3)
AIC(arima(second.diff, order=c(2,0,3)))
tsdiag(arima(second.diff, order=c(2,0,3)))

# ARMA(3,2)
AIC(arima(second.diff, order=c(3,0,2)))
tsdiag(arima(second.diff, order=c(3,0,2)))

# ARMA(3,1)
AIC(arima(second.diff, order=c(3,0,1)))
tsdiag(arima(second.diff, order=c(3,0,1)))

# An ARIMA(2,2,2) model is finalised after considering second order differencing 

# Now comparing the two models - 
arima(lerwick.temp, order=c(2,1,1))
arima(lerwick.temp, order=c(2,2,2))
tsdiag(arima(lerwick.temp, order=c(2,1,1)))
tsdiag(arima(lerwick.temp, order=c(2,2,2)))

# The AIC for model with 1st order differencing is marginally lower and is selected

# Finally, predicting the values for the first week of October - 
ler_pred <- predict(arima(lerwick.temp, order=c(2,1,1)), n.ahead = 7)
ler_pred$pred

# Lerwick predictions, tabled
library(gt)
lerwick_predplot <- data.frame(Date = c("Oct.1", "Oct.2", "Oct.3","Oct.4","Oct.5", "Oct.6", "Oct.7"), Actual =  temp_data[275:281, "Lerwick"], Predicted = as.numeric(ler_pred$pred))
gt(lerwick_predplot)%>%
  tab_header(
    title = md("**Temperature prediction**"), 
    subtitle = "Lerwick, Oct. 1-7, 2020")%>%
  cols_align(
    align = c("center"),
    columns = everything())%>%
  fmt_number(
    columns = Predicted,
    decimals = 2)

?likfit

# Assessing the fit of models by comparing predictions against actual data
# Mean Squared Error for LERWICK 
ler.mse <- mean((ler_pred$pred-(temp_data[275:281,"Lerwick"]))^2)

# Assessing this model's appropriateness for other locations - 

# Machrihanish
machri.temp <- ts(temp_data[1:274,"Machrihanish"])
machri_pred <- predict(arima(machri.temp, order=c(2,1,1)), n.ahead = 7)
machri_pred$pred
# Mean Squared Error for MACHRIHANISH
machri.mse <- mean((machri_pred$pred-(temp_data[275:281,"Machrihanish"]))^2)

# High Wycombe
hw.temp <- ts(temp_data[1:274,"High_Wycombe"])
hw_pred <- predict(arima(hw.temp, order=c(2,1,1)), n.ahead = 7)
hw_pred$pred
# Mean Squared Error for HIGH WYCOMBE
hw.mse <- mean((hw_pred$pred-(temp_data[275:281,"High_Wycombe"]))^2)

# Camborne
cam.temp <- ts(temp_data[1:274,"Camborne"])
cam_pred <- predict(arima(cam.temp, order=c(2,1,1)), n.ahead = 7)
cam_pred$pred
# Mean Squared Error for CAMBORNE
cam.mse <- mean((cam_pred$pred-(temp_data[275:281,"Camborne"]))^2)

# Dun Fell
dun.temp <- ts(temp_data[1:274,"Dun_Fell"])
dun_pred <- predict(arima(dun.temp, order=c(2,1,1)), n.ahead = 7)
dun_pred$pred
# Mean Squared Error for DUN FELL
dun.mse <- mean((dun_pred$pred-(temp_data[275:281,"Dun_Fell"]))^2)

# Plymouth
ply.temp <- ts(temp_data[1:274,"Plymouth"])
ply_pred <- predict(arima(ply.temp, order=c(2,1,1)), n.ahead = 7)
ply_pred$pred
# Mean Squared Error for PLYMOUTH
ply.mse <- mean((ply_pred$pred-(temp_data[275:281,"Plymouth"]))^2)

# Durham
dur.temp <- ts(temp_data[1:274,"Durham"])
dur_pred <- predict(arima(dur.temp, order=c(2,1,1)), n.ahead = 7)
dur_pred$pred
# Mean Squared Error for DURHAM
dur.mse <- mean((dur_pred$pred-(temp_data[275:281,"Durham"]))^2)

# London
lon.temp <- ts(temp_data[1:274,"London"])
lon_pred <- predict(arima(lon.temp, order=c(2,1,1)), n.ahead = 7)
lon_pred$pred
# Mean Squared Error for LONDON
lon.mse <- mean((lon_pred$pred-(temp_data[275:281,"London"]))^2)

# Porthmadog
por.temp <- ts(temp_data[1:274,"Porthmadog"])
por_pred <- predict(arima(por.temp, order=c(2,1,1)), n.ahead = 7)
por_pred$pred
# Mean Squared Error for PORTHMADOG
por.mse <- mean((por_pred$pred-(temp_data[275:281,"Porthmadog"]))^2)

# Leconfield
lec.temp <- ts(temp_data[1:274,"Leconfield"])
lec_pred <- predict(arima(lec.temp, order=c(2,1,1)), n.ahead = 7)
lec_pred$pred
# Mean Squared Error for DUN FELL
lec.mse <- mean((lec_pred$pred-(temp_data[275:281,"Leconfield"]))^2)

# Kinross
kin.temp <- ts(temp_data[1:274,"Kinross"])
kin_pred <- predict(arima(kin.temp, order=c(2,1,1)), n.ahead = 7)
kin_pred$pred
# Mean Squared Error for DUN FELL
kin.mse <- mean((kin_pred$pred-(temp_data[275:281,"Kinross"]))^2)

# Morecambe
more.temp <- ts(temp_data[1:274,"Morecambe"])
more_pred <- predict(arima(more.temp, order=c(2,1,1)), n.ahead = 7)
more_pred$pred
# Mean Squared Error for DUN FELL
more.mse <- mean((more_pred$pred-(temp_data[275:281,"Morecambe"]))^2)

# Lossiemouth
loss.temp <- ts(temp_data[1:274,"Lossiemouth"])
loss_pred <- predict(arima(loss.temp, order=c(2,1,1)), n.ahead = 7)
loss_pred$pred
# Mean Squared Error for DUN FELL
loss.mse <- mean((loss_pred$pred-(temp_data[275:281,"Lossiemouth"]))^2)

# Marham
mar.temp <- ts(temp_data[1:274,"Marham"])
mar_pred <- predict(arima(mar.temp, order=c(2,1,1)), n.ahead = 7)
mar_pred$pred
# Mean Squared Error for DUN FELL
mar.mse <- mean((mar_pred$pred-(temp_data[275:281,"Marham"]))^2)

# Whitby
whit.temp <- ts(temp_data[1:274,"Whitby"])
whit_pred <- predict(arima(whit.temp, order=c(2,1,1)), n.ahead = 7)
whit_pred$pred
# Mean Squared Error for DUN FELL
whit.mse <- mean((whit_pred$pred-(temp_data[275:281,"Whitby"]))^2)

# Yeovilton
yeo.temp <- ts(temp_data[1:274,"Yeovilton"])
yeo_pred <- predict(arima(yeo.temp, order=c(2,1,1)), n.ahead = 7)
yeo_pred$pred
# Mean Squared Error for DUN FELL
yeo.mse <- mean((yeo_pred$pred-(temp_data[275:281,"Yeovilton"]))^2)

# Sheffield
shef.temp <- ts(temp_data[1:274,"Sheffield"])
shef_pred <- predict(arima(shef.temp, order=c(2,1,1)), n.ahead = 7)
shef_pred$pred
# Mean Squared Error for DUN FELL
shef.mse <- mean((shef_pred$pred-(temp_data[275:281,"Sheffield"]))^2)

# Coventry
cove.temp <- ts(temp_data[1:274,"Coventry"])
cove_pred <- predict(arima(cove.temp, order=c(2,1,1)), n.ahead = 7)
cove_pred$pred
# Mean Squared Error for DUN FELL
cove.mse <- mean((cove_pred$pred-(temp_data[275:281,"Coventry"]))^2)

# Stornoway
sto.temp <- ts(temp_data[1:274,"Stornoway"])
sto_pred <- predict(arima(sto.temp, order=c(2,1,1)), n.ahead = 7)
sto_pred$pred
# Mean Squared Error for DUN FELL
sto.mse <- mean((sto_pred$pred-(temp_data[275:281,"Stornoway"]))^2)

# Lyneham
lyn.temp <- ts(temp_data[1:274,"Lyneham"])
lyn_pred <- predict(arima(lyn.temp, order=c(2,1,1)), n.ahead = 7)
lyn_pred$pred
# Mean Squared Error for DUN FELL
lyn.mse <- mean((lyn_pred$pred-(temp_data[275:281,"Lyneham"]))^2)


# Plotting the errors on a map to visualise potential spatial characteristics
library(maps)
library(viridis)
library(ggplot2)
library(mapdata)

uk_map <- map_data(map = "world", region = "UK") 
ggplot(data = uk_map, aes(x = long, y = lat)) + 
  geom_polygon() +
  coord_map()
meta_data
meta_data[15,2]
map('worldHires', "UK",xlim=c(-11,3), ylim=c(49,60.9))
points(meta_data[15,2], meta_data[15,3], pch=19,col="mediumorchid4")  # Lerwick 
points(meta_data[1,2], meta_data[1,3], pch=19,col="mediumorchid4")  # Machri 
points(meta_data[2,2], meta_data[2,3], pch=19,col="mediumorchid1")  # High Wycombe
points(meta_data[3,2], meta_data[3,3], pch=19,col="mediumorchid2")  # Camborne 
points(meta_data[4,2], meta_data[4,3], pch=19,col="mediumorchid3")  # Dun Fell
points(meta_data[5,2], meta_data[5,3], pch=19,col="mediumorchid2")   # Plymouth
points(meta_data[6,2], meta_data[6,3], pch=19,col="mediumorchid4")  # Durham 
points(meta_data[7,2], meta_data[7,3], pch=19,col="mediumorchid1")   # London
points(meta_data[8,2], meta_data[8,3], pch=19,col="mediumorchid3")  # Porthmadog 
points(meta_data[9,2], meta_data[9,3], pch=19,col="mediumorchid4")   # Leconfield
points(meta_data[10,2], meta_data[10,3], pch=19,col="mediumorchid3") # knross  
points(meta_data[11,2], meta_data[11,3], pch=19,col="mediumorchid4")  # Morecambe 
points(meta_data[12,2], meta_data[12,3], pch=19,col="mediumorchid4")  # Lossiemouth 
points(meta_data[13,2], meta_data[13,3], pch=19,col="mediumorchid2")   # Marham
points(meta_data[14,2], meta_data[14,3], pch=19,col="mediumorchid4")   # Whitby
points(meta_data[16,2], meta_data[16,3], pch=19,col="mediumorchid1")   # Yeovilton
points(meta_data[17,2], meta_data[17,3], pch=19,col="mediumorchid2")   # Sheffield
points(meta_data[18,2], meta_data[18,3], pch=19,col="mediumorchid2")   # Cove
points(meta_data[19,2], meta_data[19,3], pch=19,col="mediumorchid4")   # Stornoway
points(meta_data[20,2], meta_data[20,3], pch=19,col="mediumorchid2")  # Lyneham
title("Mean Squared Error of Maximum Daily Temperatures")



# The time-series model is clearly more appropriate for northern UK versus southern UK
# The spatio-temporal nature of temperature 

# This begs the question - can the temperature at all high-elevation areas be 



### AUGUST HEATWAVE ###

# Heatwave locations
map('worldHires', "UK",xlim=c(-11,3), ylim=c(49,60.9))
points(meta_data[7,2], meta_data[7,3], pch=19,col="mediumorchid1")   # London
points(meta_data[2,2], meta_data[2,3], pch=19,col="mediumorchid1")  # High Wycombe
points(meta_data[13,2], meta_data[13,3], pch=19,col="mediumorchid2")   # Marham

# Predicting temperature for the heatwave week using data up to and including the beginning of heatwave
# Heatwave week - 7 Aug to 12 Aug

# Using temperature data up to and not including 7 August to fit a time series model for LONDON
pre.heat <- ts(temp_data[1:219, "London"])
plot(pre.heat)
pre.dif <- diff(pre.heat, 1)
plot(pre.dif)

# The first-order differenced time series looks stationary so an ACF and PACF will be plotted for the same
par(mfrow=c(1,2))
acf(pre.dif); pacf(pre.dif)

# Both ACF and PACF tend towards zero gradually, implying that an ARMA model could be appropriate
# ARMA(1,1)
AIC(arima(pre.dif, order=c(1,0,1)))
tsdiag(arima(pre.dif, order=c(1,0,1)))

# ARMA(1,2)
AIC(arima(pre.dif, order=c(1,0,2)))
tsdiag(arima(pre.dif, order=c(1,0,2)))

# ARMA(2,1)
AIC(arima(pre.dif, order=c(2,0,1)))
tsdiag(arima(pre.dif, order=c(2,0,1)))

# ARMA(2,2)
AIC(arima(pre.dif, order=c(2,0,2)))
tsdiag(arima(pre.dif, order=c(2,0,2)))

# ARMA(2,3)
AIC(arima(pre.dif, order=c(2,0,3)))
tsdiag(arima(pre.dif, order=c(2,0,3)))

# ARMA(3,2)
AIC(arima(pre.dif, order=c(3,0,2)))
tsdiag(arima(pre.dif, order=c(3,0,2)))

# ARMA(3,1)
AIC(arima(pre.dif, order=c(3,0,1)))
tsdiag(arima(pre.dif, order=c(3,0,1)))

# An ARIMA(1,1,2) model is finalised for prediction
lonheat_pred <- predict(arima(pre.heat, order=c(1,1,2)), n.ahead = 7)
lonheat_pred$pred

# Assessing the fit of models by comparing predictions against actual data
# Mean Squared Error for LONDON HEATWAVE WEEK
lonheat.mse <- mean((lonheat_pred$pred-(temp_data[220:226,"London"]))^2)


# Using the same model to predict temperatures for the heatwave week 
# HIGH WYCOMBE
pre.heat.2 <- ts(temp_data[1:219,"High_Wycombe"])
hwheat_pred <- predict(arima(pre.heat.2, order=c(1,1,2)), n.ahead = 7)
hwheat_pred$pred
# Mean Squared Error for HIGH WYCOMBE HEAT WAVE
hwheat.mse <- mean((hwheat_pred$pred-(temp_data[220:226,"High_Wycombe"]))^2)

# MARHAM
pre.heat.3 <- ts(temp_data[1:219,"Marham"])
marheat_pred <- predict(arima(pre.heat.3, order=c(1,1,2)), n.ahead = 7)
marheat_pred$pred
# Mean Squared Error for HIGH WYCOMBE HEAT WAVE
marheat.mse <- mean((marheat_pred$pred-(temp_data[220:226,"Marham"]))^2)

# Creating plots to visualise the discrepancy in predictions
# London
vec_lon <- append(temp_data[215:219, "London"], lonheat_pred$pred)
df_lon <- data.frame(Date = temp_data[215:226, "Date"], Actual =  temp_data[215:226, "London"], Predicted = vec_lon)
df_lon
str(df_lon)
ggplot(df_lon)+
  geom_line(mapping= aes(x=as.character(Date), y=Actual), group=1, col="red")+
  geom_line(mapping=aes(x=as.character(Date), y=Predicted), group=1)+
  theme_bw()+
  xlab("Date")+
  ylab("Actual vs Predicted temperatures")
  

# High Wycombe
vec_hw <- append(temp_data[215:219, "High_Wycombe"], hwheat_pred$pred)
df_hw <- data.frame(Date = temp_data[215:226, "Date"], Actual =  temp_data[215:226, "High_Wycombe"], Predicted = vec_hw)
df_hw
ggplot(df_hw)+
  geom_line(mapping= aes(x=as.character(Date), y=Actual), group=1, col="red")+
  geom_line(mapping=aes(x=as.character(Date), y=Predicted), group=1)+
  theme_bw()+
  xlab("Date")+
  ylab("Actual vs Predicted temperatures")

# Marham
vec_mar <- append(temp_data[215:219, "Marham"], marheat_pred$pred)
df_mar <- data.frame(Date = temp_data[215:226, "Date"], Actual =  temp_data[215:226, "Marham"], Predicted = vec_mar)
df_mar
ggplot(df_mar)+
  geom_line(mapping= aes(x=as.character(Date), y=Actual), group=1, col="red")+
  geom_line(mapping=aes(x=as.character(Date), y=Predicted), group=1)+
  theme_bw()+
  xlab("Date")+
  ylab("Actual vs Predicted temperatures")


# AUGUST HEATWAVE SPATIAL MAPPING #

# Predicting the temperature on 7 August, the first day of heatwave, for London, Marham, and High Wycombe
# August 7 temperature data
aug.7 <- temp_data%>%
  filter(Date == 20200807)

# To - fit a spatial model to predict the maximum temperature in London, Marham, and 
# High Wycombe on August 7, 2020

# August 15 is the golden day so temperature data for the same will be extracted
select.aug.heat <- aug.7[,-c(1, 3, 8, 14)]

# Selecting temperature data
temp.heat <- as.numeric(select.aug.heat[1,])
# Selecting location and elevation data
loc.heat <- meta_data[-c(2, 7, 13),]
# Merging the two datasets
loc.heat$temp <- temp.heat
loc.heat
# Converting to geodata object
heat.gdat <- as.geodata(loc.heat, coords.col=2:3, data.col=5, covar.col = 4)
heat.gdat

# Plotting the temperature data
plot(heat.gdat)
# There appears to be a spatial correlation in temperature with higher temperatures 
# In the south-west, and lower temperatures in the north and north-west

plot(variog(heat.gdat))
# The variogram appears to be very noisy with few data points; it would be wise not
# to fit a model on these points

# Checking for anisotropy
plot(variog4(heat.gdat, coords=heat.gdat$coords, data=heat.gdat$data))

# MLE
# Matern 1.5 - with trend: elevation + 1st
AIC(likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Elevation+Latitude+Longitude, kappa=1.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(heat.gdat, model=likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Elevation+Latitude+Longitude, kappa=1.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Matern 1.5 - with trend: elevation + 2nd
AIC(likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Elevation+Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), kappa=1.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(heat.gdat, model=likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Elevation+Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), kappa=1.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Matern 1.5 - with trend: 1st
AIC(likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Latitude+Longitude, kappa=1.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(heat.gdat, model=likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Latitude+Longitude, kappa=1.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Matern 1.5 - with trend: 2nd
AIC(likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), kappa=1.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(heat.gdat, model=likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), kappa=1.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Matern 1.5 - without trend
AIC(likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", kappa=1.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(heat.gdat, model=likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", kappa=1.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))


# Now testing some more covariance models - 

# Matern 0.5 - with trend: elevation + 1st
AIC(likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Elevation+Latitude+Longitude, kappa=0.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(heat.gdat, model=likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Elevation+Latitude+Longitude, kappa=0.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Matern 0.5 - with trend: elevation + 2nd
AIC(likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Elevation+Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), kappa=0.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(heat.gdat, model=likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Elevation+Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), kappa=0.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Matern 0.5 - with trend: 1st
AIC(likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Latitude+Longitude, kappa=0.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(heat.gdat, model=likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Latitude+Longitude, kappa=0.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Matern 0.5 - with trend: 2nd
AIC(likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), kappa=0.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(heat.gdat, model=likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), kappa=0.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Matern 0.5 - without trend
AIC(likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", kappa=0.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(heat.gdat, model=likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", kappa=0.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))



# Squared exponential
# STAR - Squared Exponential - with trend: elevation + 1st
AIC(likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="powered.exponential", lik.met="REML",kappa=2, trend=~Elevation+Latitude+Longitude,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(heat.gdat, model=likfit(heat.gdat, ini=c(0.5, 0.5), lik.met="REML",cov.model="powered.exponential", kappa=2,trend=~Elevation+Latitude+Longitude, fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Squared Exponential - with trend: elevation + 2nd
AIC(likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="powered.exponential",kappa=2, trend=~Elevation+Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude),fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(heat.gdat, model=likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="powered.exponential", kappa=2,trend=~Elevation+Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude),fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Squared Exponential - with trend: 1st
AIC(likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="powered.exponential", trend=~Latitude+Longitude, kappa=2,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(heat.gdat, model=likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="powered.exponential", kappa=2,trend=~Latitude+Longitude, fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Squared Exponential - with trend: 2nd
AIC(likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="powered.exponential",kappa=2, trend=~Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(heat.gdat, model=likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="powered.exponential", kappa=2,trend=~Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Squared Exponential - without trend
AIC(likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="powered.exponential", fix.nugget=FALSE, kappa=2,fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(heat.gdat, model=likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="powered.exponential", kappa=2,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))


# Matern 2.5
# Matern 0.5 - with trend: elevation + 1st
AIC(likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Elevation+Latitude+Longitude, kappa=2.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(heat.gdat, model=likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Elevation+Latitude+Longitude, kappa=2.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Matern 0.5 - with trend: elevation + 2nd
AIC(likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Elevation+Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), kappa=2.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(heat.gdat, model=likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Elevation+Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), kappa=2.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Matern 0.5 - with trend: 1st
AIC(likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Latitude+Longitude, kappa=2.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(heat.gdat, model=likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Latitude+Longitude, kappa=2.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Matern 0.5 - with trend: 2nd
AIC(likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), kappa=2.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(heat.gdat, model=likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", trend=~Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), kappa=2.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Matern 0.5 - without trend
AIC(likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", kappa=2.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(heat.gdat, model=likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="matern", kappa=2.5,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))



# Exponential
# Exponential - with trend: elevation + 1st
AIC(likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="exponential", trend=~Elevation+Latitude+Longitude,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(heat.gdat, model=likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="exponential", trend=~Elevation+Latitude+Longitude, fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Exponential - with trend: elevation + 2nd
AIC(likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="exponential", trend=~Elevation+Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude),fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(heat.gdat, model=likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="exponential", trend=~Elevation+Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude),fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Exponential - with trend: 1st
AIC(likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="exponential", trend=~Latitude+Longitude, fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(heat.gdat, model=likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="exponential", trend=~Latitude+Longitude, fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Exponential - with trend: 2nd
AIC(likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="exponential", trend=~Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(heat.gdat, model=likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="exponential", trend=~Latitude+Longitude+I(Latitude^2)+I(Longitude^2)+I(Latitude*Longitude), fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Exponential - without trend
AIC(likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="exponential", fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE))
plot(xvalid(heat.gdat, model=likfit(heat.gdat, ini=c(0.5, 0.5), cov.model="exponential", fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)))

# Selecting the model EXPONENTIAL WITH TREND INCLUDING ELEVATION+1ST TREND
mod.heat <- likfit(heat.gdat, ini=c(0.5, 0.5), lik.met="REML",cov.model="exponential", trend=~Elevation+Latitude+Longitude,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)

# Now finding the mean and variance at a grid of resolution 0.1° over Britain

# First calculating the range of Latitude and Longitude to map over entire Britain
bri_grid <- expand.grid(seq(-8, 2, by=0.1), seq(48, 62, by=0.1))

# Now finding the mean and variance at points on the grid created above
heat_con <- krige.conv(heat.gdat, loc=bri_grid, krige=krige.control(obj.model=mod.heat))

# Plotting the means and variances
map('worldHires', "UK",xlim=c(-11,3), ylim=c(49,60.9))
image(heat_con, col = viridis::plasma(15), coords.data = heat.gdat[1]$coords, main = 'Mean - ML')
image(heat_con, values = heat_con$krige.var, coords.data = heat.gdat[1]$coords,
      main = 'Variance - ML')
pimage(bri_grid[,1],bri_grid[,2],heat_con$predict, col = viridis::plasma(25), legend="vertical", map="world", proj="bonne", parameters=45, points=list(x=as.numeric(heat.gdat$coords[,1]),y= as.numeric(heat.gdat$coords[,2])), points.args = list(pch = 19, col = "cyan", cex=0.5))
?krige.conv

# Now, predicting the maximum daily temperature for Leconfield, Yeovilton, and High Wycombe 

# Firstly, creating a dataframe containing the locations
heat_pred <- meta_data[c(2, 7, 13),-c(1,4)]
heat_con.pred <- krige.conv(heat.gdat, loc=heat_pred, krige=krige.control(obj.model=mod.heat))
heat_con.pred
heat_con.pred$predict[2]
# August 8 temperature data
aug.8 <- temp_data%>%
  filter(Date == 20200808)

# To - fit a spatial model to predict the maximum temperature in London, Marham, and 
# High Wycombe on August 8, 2020

# August 15 is the golden day so temperature data for the same will be extracted
select.aug.heat2 <- aug.8[,-c(1, 3, 8, 14)]

# Selecting temperature data
temp.heat2 <- as.numeric(select.aug.heat2[1,])
# Selecting location and elevation data
loc.heat2 <- meta_data[-c(2, 7, 13),]
# Merging the two datasets
loc.heat2$temp <- temp.heat2
loc.heat2
# Converting to geodata object
heat.gdat2 <- as.geodata(loc.heat2, coords.col=2:3, data.col=5, covar.col = 4)
heat.gdat2

# Plotting the temperature data
plot(heat.gdat2)
# There appears to be a spatial correlation in temperature with higher temperatures 
# In the south-west, and lower temperatures in the north and north-west
?likfit
# Selecting the model EXPONENTIAL WITH TREND INCLUDING ELEVATION+1ST TREND
mod.heat2 <- likfit(heat.gdat2, ini=c(0.5, 0.5), lik.method="REML",cov.model="matern", kappa=2.5, trend=~Elevation+Latitude+Longitude,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)

# Now finding the mean and variance at a grid of resolution 0.1° over Britain

# First calculating the range of Latitude and Longitude to map over entire Britain
bri_grid <- expand.grid(seq(-8, 2, by=0.1), seq(48, 62, by=0.1))

# Now finding the mean and variance at points on the grid created above
heat_con2 <- krige.conv(heat.gdat2, loc=bri_grid, krige=krige.control(obj.model=mod.heat))

# Plotting the means and variances
map('worldHires', "UK",xlim=c(-11,3), ylim=c(49,60.9))
image(heat_con2, col = viridis::plasma(15), coords.data = heat.gdat2[1]$coords, main = 'Mean - ML')
image(heat_con2, values = heat_con2$krige.var, coords.data = heat.gdat2[1]$coords,
      main = 'Variance - ML')
pimage(bri_grid[,1],bri_grid[,2],heat_con2$predict, col = viridis::plasma(25), legend="vertical", map="world", proj="bonne", parameters=45, points=list(x=as.numeric(heat.gdat$coords[,1]),y= as.numeric(heat.gdat$coords[,2])), points.args = list(pch = 19, col = "cyan", cex=0.5))
?krige.conv
?krige.conv

# Now, predicting the maximum daily temperature for Leconfield, Yeovilton, and High Wycombe 

# Firstly, creating a dataframe containing the locations
heat_pred2 <- meta_data[c(2, 7, 13),-c(1,4)]
heat_con.pred2 <- krige.conv(heat.gdat2, loc=heat_pred2, krige=krige.control(obj.model=mod.heat))
heat_con.pred2


## August 9 
aug.9 <- temp_data%>%
  filter(Date == 20200809)

# To - fit a spatial model to predict the maximum temperature in London, Marham, and 
# High Wycombe on August 9, 2020

# August 15 is the golden day so temperature data for the same will be extracted
select.aug.heat3 <- aug.9[,-c(1, 3, 8, 14)]

# Selecting temperature data
temp.heat3 <- as.numeric(select.aug.heat3[1,])
# Selecting location and elevation data
loc.heat3 <- meta_data[-c(2, 7, 13),]
# Merging the two datasets
loc.heat3$temp <- temp.heat3
loc.heat3
# Converting to geodata object
heat.gdat3 <- as.geodata(loc.heat3, coords.col=2:3, data.col=5, covar.col = 4)
heat.gdat3

# Plotting the temperature data
plot(heat.gdat3)
# There appears to be a spatial correlation in temperature with higher temperatures 
# In the south-west, and lower temperatures in the north and north-west
?likfit
# Selecting the model EXPONENTIAL WITH TREND INCLUDING ELEVATION+1ST TREND
mod.heat3 <- likfit(heat.gdat3, ini=c(0.5, 0.5), lik.method="REML",cov.model="matern", kappa=2.5, trend=~Elevation+Latitude+Longitude,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)

# Now finding the mean and variance at a grid of resolution 0.1° over Britain

# First calculating the range of Latitude and Longitude to map over entire Britain
bri_grid <- expand.grid(seq(-8, 2, by=0.1), seq(48, 62, by=0.1))

# Now finding the mean and variance at points on the grid created above
heat_con3 <- krige.conv(heat.gdat3, loc=bri_grid, krige=krige.control(obj.model=mod.heat))

# Plotting the means and variances
map('worldHires', "UK",xlim=c(-11,3), ylim=c(49,60.9))
image(heat_con3, col = viridis::plasma(15), coords.data = heat.gdat3[1]$coords, main = 'Mean - ML')
image(heat_con3, values = heat_con3$krige.var, coords.data = heat.gdat3[1]$coords,
      main = 'Variance - ML')
library(autoimage)
pimage(bri_grid[,1],bri_grid[,2],heat_con3$predict, col = viridis::plasma(25), legend="vertical", map="world", proj="bonne", parameters=45, points=list(x=as.numeric(heat.gdat$coords[,1]),y= as.numeric(heat.gdat$coords[,2])), points.args = list(pch = 19, col = "cyan", cex=0.5))
?krige.conv

# Now, predicting the maximum daily temperature for Leconfield, Yeovilton, and High Wycombe 

# Firstly, creating a dataframe containing the locations
heat_pred3 <- meta_data[c(2, 7, 13),-c(1,4)]
heat_con.pred3 <- krige.conv(heat.gdat3, loc=heat_pred3, krige=krige.control(obj.model=mod.heat))
heat_con.pred3

## August 10
aug.10 <- temp_data%>%
  filter(Date == 20200810)

# To - fit a spatial model to predict the maximum temperature in London, Marham, and 
# High Wycombe on August 10, 2020

# August 15 is the golden day so temperature data for the same will be extracted
select.aug.heat4 <- aug.10[,-c(1, 3, 8, 14)]

# Selecting temperature data
temp.heat4 <- as.numeric(select.aug.heat4[1,])
# Selecting location and elevation data
loc.heat4 <- meta_data[-c(2, 7, 13),]
# Merging the two datasets
loc.heat4$temp <- temp.heat4
loc.heat4
# Converting to geodata object
heat.gdat4 <- as.geodata(loc.heat4, coords.col=2:3, data.col=5, covar.col = 4)
heat.gdat4

# Plotting the temperature data
plot(heat.gdat4)
# There appears to be a spatial correlation in temperature with higher temperatures 
# In the south-west, and lower temperatures in the north and north-west
?likfit
# Selecting the model EXPONENTIAL WITH TREND INCLUDING ELEVATION+1ST TREND
mod.heat4 <- likfit(heat.gdat4, ini=c(0.5, 0.5), lik.method="REML",cov.model="matern", kappa=2.5, trend=~Elevation+Latitude+Longitude,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)

# Now finding the mean and variance at a grid of resolution 0.1° over Britain

# First calculating the range of Latitude and Longitude to map over entire Britain
bri_grid <- expand.grid(seq(-8, 2, by=0.1), seq(48, 62, by=0.1))

# Now finding the mean and variance at points on the grid created above
heat_con4 <- krige.conv(heat.gdat4, loc=bri_grid, krige=krige.control(obj.model=mod.heat))

# Plotting the means and variances
map('worldHires', "UK",xlim=c(-11,3), ylim=c(49,60.9))
image(heat_con4, col = viridis::plasma(15), coords.data = heat.gdat4[1]$coords, main = 'Mean - ML')
image(heat_con4, values = heat_con4$krige.var, coords.data = heat.gdat4[1]$coords,
      main = 'Variance - ML')
pimage(bri_grid[,1],bri_grid[,2],heat_con4$predict, col = viridis::plasma(25), legend="vertical", map="world", proj="bonne", parameters=45, points=list(x=as.numeric(heat.gdat$coords[,1]),y= as.numeric(heat.gdat$coords[,2])), points.args = list(pch = 19, col = "cyan", cex=0.5))

?krige.conv

# Now, predicting the maximum daily temperature for Leconfield, Yeovilton, and High Wycombe 

# Firstly, creating a dataframe containing the locations
heat_pred4 <- meta_data[c(2, 7, 13),-c(1,4)]
heat_con.pred4 <- krige.conv(heat.gdat4, loc=heat_pred4, krige=krige.control(obj.model=mod.heat))
heat_con.pred4

## August 11
aug.11 <- temp_data%>%
  filter(Date == 20200811)

# To - fit a spatial model to predict the maximum temperature in London, Marham, and 
# High Wycombe on August 10, 2020

# August 15 is the golden day so temperature data for the same will be extracted
select.aug.heat5 <- aug.11[,-c(1, 3, 8, 14)]

# Selecting temperature data
temp.heat5 <- as.numeric(select.aug.heat5[1,])
# Selecting location and elevation data
loc.heat5 <- meta_data[-c(2, 7, 13),]
# Merging the two datasets
loc.heat5$temp <- temp.heat5
loc.heat5
# Converting to geodata object
heat.gdat5 <- as.geodata(loc.heat5, coords.col=2:3, data.col=5, covar.col = 4)
heat.gdat5

# Plotting the temperature data
plot(heat.gdat5)
# There appears to be a spatial correlation in temperature with higher temperatures 
# In the south-west, and lower temperatures in the north and north-west

# Selecting the model EXPONENTIAL WITH TREND INCLUDING ELEVATION+1ST TREND
mod.heat5 <- likfit(heat.gdat5, ini=c(0.5, 0.5), lik.method="REML",cov.model="matern", kappa=2.5, trend=~Elevation+Latitude+Longitude,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)

# Now finding the mean and variance at a grid of resolution 0.1° over Britain

# First calculating the range of Latitude and Longitude to map over entire Britain
bri_grid <- expand.grid(seq(-8, 2, by=0.1), seq(48, 62, by=0.1))

# Now finding the mean and variance at points on the grid created above
heat_con5 <- krige.conv(heat.gdat5, loc=bri_grid, krige=krige.control(obj.model=mod.heat))

# Plotting the means and variances
map('worldHires', "UK",xlim=c(-11,3), ylim=c(49,60.9))
image(heat_con5, col = viridis::plasma(15), coords.data = heat.gdat5[1]$coords, main = 'Mean - ML')
image(heat_con5, values = heat_con5$krige.var, coords.data = heat.gdat5[1]$coords,
      main = 'Variance - ML')
pimage(bri_grid[,1],bri_grid[,2],heat_con5$predict, col = viridis::plasma(25), legend="vertical", map="world", proj="bonne", parameters=45, points=list(x=as.numeric(heat.gdat$coords[,1]),y= as.numeric(heat.gdat$coords[,2])), points.args = list(pch = 19, col = "cyan", cex=0.5))

# Now, predicting the maximum daily temperature for Leconfield, Yeovilton, and High Wycombe 

# Firstly, creating a dataframe containing the locations
heat_pred5 <- meta_data[c(2, 7, 13),-c(1,4)]
heat_con.pred5 <- krige.conv(heat.gdat5, loc=heat_pred5, krige=krige.control(obj.model=mod.heat))
heat_con.pred5

## August 12
aug.12 <- temp_data%>%
  filter(Date == 20200812)

# To - fit a spatial model to predict the maximum temperature in London, Marham, and 
# High Wycombe on August 10, 2020

# August 15 is the golden day so temperature data for the same will be extracted
select.aug.heat6 <- aug.12[,-c(1, 3, 8, 14)]

# Selecting temperature data
temp.heat6 <- as.numeric(select.aug.heat6[1,])
# Selecting location and elevation data
loc.heat6 <- meta_data[-c(2, 7, 13),]
# Merging the two datasets
loc.heat6$temp <- temp.heat6
loc.heat6
# Converting to geodata object
heat.gdat6 <- as.geodata(loc.heat6, coords.col=2:3, data.col=5, covar.col = 4)
heat.gdat6

# Plotting the temperature data
plot(heat.gdat6)
# There appears to be a spatial correlation in temperature with higher temperatures 
# In the south-west, and lower temperatures in the north and north-west

# Selecting the model EXPONENTIAL WITH TREND INCLUDING ELEVATION+1ST TREND
mod.heat6 <- likfit(heat.gdat6, ini=c(0.5, 0.5), lik.method="REML",cov.model="matern", kappa=2.5, trend=~Elevation+Latitude+Longitude,fix.nugget=FALSE, fix.psiA = FALSE, fix.psiR = FALSE)

# Now finding the mean and variance at a grid of resolution 0.1° over Britain

# First calculating the range of Latitude and Longitude to map over entire Britain
bri_grid <- expand.grid(seq(-8, 2, by=0.1), seq(48, 62, by=0.1))

# Now finding the mean and variance at points on the grid created above
heat_con6 <- krige.conv(heat.gdat6, loc=bri_grid, krige=krige.control(obj.model=mod.heat))

# Plotting the means and variances
map('worldHires', "UK",xlim=c(-11,3), ylim=c(49,60.9))
image(heat_con6, col = viridis::plasma(15), coords.data = heat.gdat6[1]$coords, main = 'Mean - ML')
image(heat_con6, values = heat_con6$krige.var, coords.data = heat.gdat6[1]$coords,
      main = 'Variance - ML')
pimage(bri_grid[,1],bri_grid[,2],heat_con6$predict, col = viridis::plasma(25), legend="vertical", map="world", proj="bonne", parameters=45, points=list(x=as.numeric(heat.gdat$coords[,1]),y= as.numeric(heat.gdat$coords[,2])), points.args = list(pch = 19, col = "cyan", cex=0.5))

# Now, predicting the maximum daily temperature for Leconfield, Yeovilton, and High Wycombe 

# Firstly, creating a dataframe containing the locations
heat_pred6 <- meta_data[c(2, 7, 13),-c(1,4)]
heat_con.pred6 <- krige.conv(heat.gdat6, loc=heat_pred6, krige=krige.control(obj.model=mod.heat))
heat_con.pred6

################ COMPARISON OF ACTUAL VS PREDICTED TEMPERATURES ###################

######### SPATIAL PREDICTIONS ############

# LONDON PREDICTIONS VS ACTUAL HEATWAVE USING SPATIAL PREDICTIONS
library(ggplot2)
london_heat_df <- data.frame(actual = temp_data[220:225,"London"])
london_heat_df$prediction <- c(heat_con.pred$predict[2], heat_con.pred2$predict[2], heat_con.pred3$predict[2], heat_con.pred4$predict[2], heat_con.pred5$predict[2], heat_con.pred6$predict[2]) 
london_heat_df$sq.error <- (london_heat_df$actual-london_heat_df$prediction)^2
london_heat_df$se <- c(sqrt(heat_con.pred$krige.var[2]), sqrt(heat_con.pred2$krige.var[2]), sqrt(heat_con.pred3$krige.var[2]), sqrt(heat_con.pred4$krige.var[2]), sqrt(heat_con.pred5$krige.var[2]), sqrt(heat_con.pred6$krige.var[2])) 
london_heat_df$u <- london_heat_df$prediction+qnorm(0.95, sd = london_heat_df$se)
london_heat_df$l <- london_heat_df$prediction+qnorm(0.05, sd=london_heat_df$se)
london_heat_df$date <- temp_data[220:225,1]
london_heat_df

ggplot(london_heat_df)+
  geom_line(mapping=aes(x=date, y=prediction, group=1))+
  geom_line(mapping=aes(x=date, y=actual, group=1), color="red")+
  geom_line(mapping=aes(x=date, y=u, group=1), linetype="dashed", color="blue")+
  geom_line(mapping=aes(x=date, y=l, group=1),linetype="dashed", color="blue")+
  theme_bw()+
  xlab("Date")+
  ylab("°C")


# HIGH WYCOMBE PREDICTIONS VS ACTUAL HEATWAVE USING SPATIAL PREDICTIONS
library(ggplot2)
hw_heat_df <- data.frame(actual = temp_data[220:225,"High_Wycombe"])
hw_heat_df$prediction <- c(heat_con.pred$predict[1], heat_con.pred2$predict[1], heat_con.pred3$predict[1], heat_con.pred4$predict[1], heat_con.pred5$predict[1], heat_con.pred6$predict[1]) 
hw_heat_df$sq.error <- (hw_heat_df$actual-hw_heat_df$prediction)^2
hw_heat_df$se <- c(sqrt(heat_con.pred$krige.var[1]), sqrt(heat_con.pred2$krige.var[1]), sqrt(heat_con.pred3$krige.var[1]), sqrt(heat_con.pred4$krige.var[1]), sqrt(heat_con.pred5$krige.var[1]), sqrt(heat_con.pred6$krige.var[1])) 
hw_heat_df$u <- hw_heat_df$prediction+qnorm(0.95, sd = hw_heat_df$se)
hw_heat_df$l <- hw_heat_df$prediction+qnorm(0.05, sd=hw_heat_df$se)
hw_heat_df$date <- temp_data[220:225,1]
hw_heat_df

ggplot(hw_heat_df)+
  geom_line(mapping=aes(x=date, y=prediction, group=1))+
  geom_line(mapping=aes(x=date, y=actual, group=1), color="red")+
  geom_line(mapping=aes(x=date, y=u, group=1), linetype="dashed", color="blue")+
  geom_line(mapping=aes(x=date, y=l, group=1),linetype="dashed", color="blue")+
  theme_bw()+
  xlab("Date")+
  ylab("°C")


# MARHAM PREDICTIONS VS ACTUAL HEATWAVE USING SPATIAL PREDICTIONS
library(ggplot2)
mar_heat_df <- data.frame(actual = temp_data[220:225,"Marham"])
mar_heat_df$prediction <- c(heat_con.pred$predict[3], heat_con.pred2$predict[3], heat_con.pred3$predict[3], heat_con.pred4$predict[3], heat_con.pred5$predict[3], heat_con.pred6$predict[3]) 
mar_heat_df$sq.error <- (mar_heat_df$actual-mar_heat_df$prediction)^2
mar_heat_df$se <- c(sqrt(heat_con.pred$krige.var[3]), sqrt(heat_con.pred2$krige.var[3]), sqrt(heat_con.pred3$krige.var[3]), sqrt(heat_con.pred4$krige.var[3]), sqrt(heat_con.pred5$krige.var[3]), sqrt(heat_con.pred6$krige.var[3])) 
mar_heat_df$u <- mar_heat_df$prediction+qnorm(0.95, sd = mar_heat_df$se)
mar_heat_df$l <- mar_heat_df$prediction+qnorm(0.05, sd=mar_heat_df$se)
mar_heat_df$date <- temp_data[220:225,1]
mar_heat_df

ggplot(mar_heat_df)+
  geom_line(mapping=aes(x=date, y=prediction, group=1))+
  geom_line(mapping=aes(x=date, y=actual, group=1), color="red")+
  geom_line(mapping=aes(x=date, y=u, group=1), linetype="dashed", color="blue")+
  geom_line(mapping=aes(x=date, y=l, group=1),linetype="dashed", color="blue")+
  theme_bw()+
  xlab("Date")+
  ylab("°C")

