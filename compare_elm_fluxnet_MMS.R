#To compare ELM simulated and Fluxnet measured variables @ US-MMS
library(ggplot2)
library(ggprism)
library(patchwork)
library(hrbrthemes)
library(ncdf4)
#virginica #versicolor #setosa
my_data <- read.csv("MMS_FLX.csv")
nc_data <- nc_open("SoutheastUS_US-MMS_ICB20TRCNPRDCTCBC.elm.h0.1998_2014.nc")
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)


#onef <- my_data$LE_F_MDS
par(mfrow=c(3,3),tcl=-0.5, family="serif", mai=c(0.2,0.9,0.2,0.2))

onef <- my_data$TA_F_MDS #Air temperature, gapfilled using MDS method
onef <- my_data$TA_ERA #Air temperature, downscaled from ERA, linearly regressed using measured only site dataS method
onef <- my_data$NEE_CUT_REF#Net Ecosystem Exchange, using Constant Ustar Threshold (CUT) across years, 
onef <- my_data$WS_F#Wind speed, consolidated from WS and WS_ERA
onef <- my_data$WS_F#Wind speed, consolidated from WS and WS_ERA
onef <- my_data$SW_IN_F
onef <- my_data$H_F_MDS
onef <- my_data$LE_F_MDS


t <- ncvar_get(nc_data, "time")

head(lon) # look at the first few entries in the longitude vector
head(lat) # look at the first few entries in the longitude vector
fillvalue <- ncatt_get(nc_data, "ZWT", "9.999999616903162e+35")
elm <- ncvar_get(nc_data, "EFLX_LH_TOT")
elm <- elm[362:6205]
df <- data.frame(as.vector(elm), as.vector(onef))


# Scatter plot and linear regression line
y <- elm#[365:2922]
x <- onef#[365:2922]
plot(x, y, col="palegreen3", pch = 16,main='Latent heat (W/m^2)',
     xlab='ONEFLUX', ylab='ELM',ylim=c(0,200),xlim=c(0,200),xaxs="i",yaxs="i",cex.lab = 1.2)
abline(lm(y ~ x), col = 1, lwd = 1.5)

# Text
coef <- round(coef(lm(y ~ x)), 2)
text(40, 180,  paste("Y = ", coef[2], "x", "+", coef[1] ))
res2 <-cor.test(x,y,  method = "spearman")
res2
cor(x, y, method = c("pearson", "kendall", "spearman")) 
cor.test(x, y, method=c("pearson", "kendall", "spearman"))

onef <- my_data$TA_F_MDS #Air temperature, gapfilled using MDS method
onef <- my_data$TA_ERA #Air temperature, downscaled from ERA, linearly regressed using measured only site dataS method
onef <- my_data$NEE_CUT_REF#Net Ecosystem Exchange, using Constant Ustar Threshold (CUT) across years, 
onef <- my_data$WS_F#Wind speed, consolidated from WS and WS_ERA
onef <- my_data$WS_F#Wind speed, consolidated from WS and WS_ERA
onef <- my_data$SW_IN_F
onef <- my_data$H_F_MDS



t <- ncvar_get(nc_data, "time")

head(lon) # look at the first few entries in the longitude vector
head(lat) # look at the first few entries in the longitude vector
fillvalue <- ncatt_get(nc_data, "ZWT", "9.999999616903162e+35")
elm <- ncvar_get(nc_data, "FSH")
elm <- elm[362:6205]
df <- data.frame(as.vector(elm), as.vector(onef))


# Scatter plot and linear regression line
y <- elm#[365:2922]
x <- onef#[365:2922]
plot(x, y, col="deepskyblue", pch = 16,main='Sensible heat (W/m^2)',
     xlab='ONEFLUX', ylab='ELM',ylim=c(0,200),xlim=c(0,200),xaxs="i",yaxs="i",cex.lab = 1.2)
abline(lm(y ~ x), col = 1, lwd = 1.5)

# Text
coef <- round(coef(lm(y ~ x)), 2)
text(40, 180,  paste("Y = ", coef[2], "x", "+", coef[1] ))
res2 <-cor.test(x,y,  method = "spearman")
res2
cor(x, y, method = c("pearson", "kendall", "spearman")) 
cor.test(x, y, method=c("pearson", "kendall", "spearman"))

onef <- my_data$TA_F_MDS #Air temperature, gapfilled using MDS method
onef <- my_data$TA_ERA #Air temperature, downscaled from ERA, linearly regressed using measured only site dataS method
onef <- my_data$NEE_CUT_REF#Net Ecosystem Exchange, using Constant Ustar Threshold (CUT) across years, 
onef <- my_data$WS_F#Wind speed, consolidated from WS and WS_ERA
onef <- my_data$WS_F#Wind speed, consolidated from WS and WS_ERA
onef <- my_data$SW_IN_F


t <- ncvar_get(nc_data, "time")

head(lon) # look at the first few entries in the longitude vector
head(lat) # look at the first few entries in the longitude vector
fillvalue <- ncatt_get(nc_data, "ZWT", "9.999999616903162e+35")
elm <- ncvar_get(nc_data, "FSDS")
elm <- elm[362:6205]
df <- data.frame(as.vector(elm), as.vector(onef))


# Scatter plot and linear regression line
y <- elm#[365:2922]
x <- onef#[365:2922]
plot(x, y, col="darkolivegreen3", pch = 16,main='Shortwave incoming (W/m^2)',
     xlab='ONEFLUX', ylab='ELM',ylim=c(0,400),xlim=c(0,400),xaxs="i",yaxs="i",cex.lab = 1.2)
abline(lm(y ~ x), col = 1, lwd = 1.5)

# Text
coef <- round(coef(lm(y ~ x)), 2)
text(100, 380,  paste("Y = ", coef[2], "x", "+", coef[1] ))
res2 <-cor.test(x,y,  method = "spearman")
res2
cor(x, y, method = c("pearson", "kendall", "spearman")) 
cor.test(x, y, method=c("pearson", "kendall", "spearman"))

onef <- my_data$TA_F_MDS #Air temperature, gapfilled using MDS method
onef <- my_data$TA_ERA #Air temperature, downscaled from ERA, linearly regressed using measured only site dataS method
onef <- my_data$NEE_CUT_REF#Net Ecosystem Exchange, using Constant Ustar Threshold (CUT) across years, 
onef <- my_data$WS_F#Wind speed, consolidated from WS and WS_ERA
onef <- my_data$WS_F#Wind speed, consolidated from WS and WS_ERA
onef <- my_data$SW_IN_F
onef1 <- my_data$H_F_MDS
onef2 <- my_data$LE_F_MDS
onef <- onef1/onef2

t <- ncvar_get(nc_data, "time")

head(lon) # look at the first few entries in the longitude vector
head(lat) # look at the first few entries in the longitude vector
fillvalue <- ncatt_get(nc_data, "ZWT", "9.999999616903162e+35")
elm1 <- ncvar_get(nc_data, "EFLX_LH_TOT")
elm2 <- ncvar_get(nc_data, "FSH")
elm <- elm2/elm1
elm <- elm[362:6205]
df <- data.frame(as.vector(elm), as.vector(onef))


# Scatter plot and linear regression line
y <- elm#[365:2922]
x <- onef#[365:2922]
plot(x, y, col="cornflowerblue", pch = 16,main='Bowen ratio',
     xlab='ONEFLUX', ylab='ELM',ylim=c(0,15),xlim=c(0,15),xaxs="i",yaxs="i",cex.lab = 1.2)
abline(lm(y ~ x), col = 1, lwd = 1.5)

# Text
coef <- round(coef(lm(y ~ x)), 2)
text(3, 8,  paste("Y = ", coef[2], "x", "+", coef[1] ))
res2 <-cor.test(x,y,  method = "spearman")
res2
cor(x, y, method = c("pearson", "kendall", "spearman")) 
cor.test(x, y, method=c("pearson", "kendall", "spearman"))

onef <- my_data$TA_F_MDS #Air temperature, gapfilled using MDS method
#onef <- onef-273


t <- ncvar_get(nc_data, "time")

head(lon) # look at the first few entries in the longitude vector
head(lat) # look at the first few entries in the longitude vector
fillvalue <- ncatt_get(nc_data, "ZWT", "9.999999616903162e+35")
elm1 <- ncvar_get(nc_data, "TBOT")
elm <- elm1-273
y <- elm[362:6205]




x <- onef#[365:2922]
plot(x, y, col="cadetblue3", pch = 16,main='Temperature (degC)',
     xlab='ONEFLUX', ylab='ELM',ylim=c(0,40),xlim=c(0,40),xaxs="i",yaxs="i",cex.lab = 1.2)
abline(lm(y ~ x), col = 1, lwd = 0)

# Text
coef <- round(coef(lm(y ~ x)), 2)
text(10, 35,  paste("Y = ", coef[2], "x", "+", coef[1] ))
res2 <-cor.test(x,y,  method = "spearman")
res2
cor(x, y, method = c("pearson", "kendall", "spearman")) 
cor.test(x, y, method=c("pearson", "kendall", "spearman"))


onef <- my_data$P_F #Air temperature, gapfilled using MDS method
#onef <- onef-273


t <- ncvar_get(nc_data, "time")

head(lon) # look at the first few entries in the longitude vector
head(lat) # look at the first few entries in the longitude vector
fillvalue <- ncatt_get(nc_data, "ZWT", "9.999999616903162e+35")
elm <- ncvar_get(nc_data, "RAIN")
elm <- elm*3600*24
elm <- elm[362:6205]
df <- data.frame(as.vector(elm), as.vector(onef))


# Scatter plot and linear regression line
y <- elm#[365:2922]
x <- onef#[365:2922]
plot(x, y, col="aquamarine3", pch = 16,main='Rainfall (mm/day)',
     xlab='ONEFLUX', ylab='ELM',ylim=c(0,40),xlim=c(0,40),xaxs="i",yaxs="i",cex.lab = 1.2)
abline(lm(y ~ x), col = 1, lwd = 1.5)

# Text
coef <- round(coef(lm(y ~ x)), 2)
text(10, 35,  paste("Y = ", coef[2], "x", "+", coef[1] ))
res2 <-cor.test(x,y,  method = "spearman")
res2
cor(x, y, method = c("pearson", "kendall", "spearman")) 
cor.test(x, y, method=c("pearson", "kendall", "spearman"))

onef <- my_data$GPP_NT_VUT_MEAN #Air temperature, gapfilled using MDS method
#onef <- onef-273


t <- ncvar_get(nc_data, "time")

head(lon) # look at the first few entries in the longitude vector
head(lat) # look at the first few entries in the longitude vector
fillvalue <- ncatt_get(nc_data, "ZWT", "9.999999616903162e+35")
elm <- ncvar_get(nc_data, "GPP")
elm <- elm*3600*24
y <- elm[362:6205]




x <- onef#[365:2922]
plot(x, y, col="cadetblue3", pch = 16,main='GPP',
     xlab='ONEFLUX', ylab='ELM',ylim=c(0,20),xlim=c(0,20),xaxs="i",yaxs="i",cex.lab = 1.2)
abline(lm(y ~ x), col = 1, lwd = 0)

# Text
coef <- round(coef(lm(y ~ x)), 2)
text(10,18,  paste("Y = ", coef[2], "x", "+", coef[1] ))
res2 <-cor.test(x,y,  method = "spearman")
res2
cor(x, y, method = c("pearson", "kendall", "spearman")) 
cor.test(x, y, method=c("pearson", "kendall", "spearman"))


onef <- my_data$NEE_CUT_REF #Air temperature, gapfilled using MDS method
#onef <- onef-273


t <- ncvar_get(nc_data, "time")

head(lon) # look at the first few entries in the longitude vector
head(lat) # look at the first few entries in the longitude vector
fillvalue <- ncatt_get(nc_data, "ZWT", "9.999999616903162e+35")
elm <- ncvar_get(nc_data, "NEE")
elm <- elm*3600*24
elm <- elm[362:6205]
df <- data.frame(as.vector(elm), as.vector(onef))


# Scatter plot and linear regression line
y <- elm#[365:2922]
x <- onef#[365:2922]
plot(x, y, col="aquamarine3", pch = 16,main='NEE',
     xlab='ONEFLUX', ylab='ELM',ylim=c(0,20),xlim=c(0,20),xaxs="i",yaxs="i",cex.lab = 1.2)
abline(lm(y ~ x), col = 1, lwd = 1.5)

# Text
coef <- round(coef(lm(y ~ x)), 2)
text(10,18,  paste("Y = ", coef[2], "x", "+", coef[1] ))
res2 <-cor.test(x,y,  method = "spearman")
res2
cor(x, y, method = c("pearson", "kendall", "spearman")) 
cor.test(x, y, method=c("pearson", "kendall", "spearman"))