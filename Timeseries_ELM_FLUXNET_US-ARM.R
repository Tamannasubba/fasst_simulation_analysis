# to plot the time series of ELM simulation outputs along with FLUXNET measurements
# change the variable name as required
library(ggplot2)
library(ggprism)
library(patchwork)
library(hrbrthemes)
library(ncdf4)
#virginica #versicolor #setosa
my_data <- read.csv("ARM_FLX.csv")
nc_data <- nc_open("SoutheastUS_US-ARM_ICB20TRCNPRDCTCBC.elm.h0.2000_2014.nc")
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)


#onef <- my_data$LE_F_MDS
par(mfrow=c(3,3),tcl=-0.5, family="serif", mai=c(0.2,0.9,0.2,0.2))

onef <- my_data$TA_F_MDS #Air temperature, gapfilled using MDS method
onef <- my_data$TA_ERA #Air temperature, downscaled from ERA, linearly regressed using measured only site dataS method
onef <- my_data$GPP_CUT_REF#Net Ecosystem Exchange, using Constant Ustar Threshold (CUT) across years, 
onef <- my_data$WS_F#Wind speed, consolidated from WS and WS_ERA
onef <- my_data$WS_F#Wind speed, consolidated from WS and WS_ERA
onef <- my_data$SW_IN_F
onef <- my_data$H_F_MDS
onef <- my_data$LE_F_MDS
onef1 <- my_data$GPP_DT_VUT_MEAN
onef2<- my_data$GPP_NT_VUT_MEAN
onef <- (onef1+onef2)/2

t <- ncvar_get(nc_data, "time")

head(lon) # look at the first few entries in the longitude vector
head(lat) # look at the first few entries in the longitude vector
fillvalue <- ncatt_get(nc_data, "ZWT", "9.999999616903162e+35")
elm <- ncvar_get(nc_data, "EFLX_LH_TOT")
elm <- ncvar_get(nc_data, "GPP")
elm <- elm*24*3600
elm <- elm[1095:4747]
df <- data.frame(as.vector(elm), as.vector(onef))


# Scatter plot and linear regression line
y <- elm#[365:2922]
x <- onef#[365:2922]
plot(x, y, col="palegreen3", pch = 16,main='GPP',
     xlab='ONEFLUX', ylab='ELM',ylim=c(0,20),xlim=c(0,20),xaxs="i",yaxs="i",cex.lab = 1.2)
abline(lm(y ~ x), col = 1, lwd = 1.5)
# Text
coef <- round(coef(lm(y ~ x)), 2)
text(40, 180,  paste("Y = ", coef[2], "x", "+", coef[1] ))
res2 <-cor.test(x,y,  method = "spearman")
res2
cor(x, y, method = c("pearson", "kendall", "spearman")) 
cor.test(x, y, method=c("pearson", "kendall", "spearman"))
###########%%%%%%%%%plot series###########%%%%%%%%%###########%%%%%%%%%###########%%%%%%%%%

start_date <- as.Date("20030101",format="%Y%m%d")          # Create example date

#Now, we can create a range of dates with the seq function as follows:

usedate <- seq(start_date, by = "day", length.out = 3653)  # Get Range with 5 dates
#2020-10-05" "2020-10-06" "2020-10-07" "2020-10-08" "2020-10-09"
#usedate = as.Date(as.character(dates),format="%Y%m%d")


df <- data.frame(date = usedate,
                 valuee = elm, valueo = onef)

ggplot() + 
  geom_point(data = df, aes(x = date, y = valuee), color = "blue1",shape=20,size=2) +
  geom_point(data = df, aes(x = date, y = valueo), color = "brown1",size=2,shape=20) +
  #theme_prism() + 
  #theme_bw() +
  #theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  scale_fill_grey() +
  labs(x = "Month-year")+
  # labs(y = "Latent heat (W/m^2)")+
  labs(x = "Month-year")+
  ylab(bquote(GPP))+
  theme(legend.position="none")+
  theme_bw(base_size = 16) +
  ggtitle("GPP (gC m-2 d-1)")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(guide = "prism_minor", 
                     limits = c(0,20),
                     expand = c(0, 0),
                     minor_breaks = seq(0,20, 2))+
  scale_x_date(limits = as.Date(c("2003-01-01","2012-12-31")), expand = c(0,0), date_labels = "%b-%Y",
               breaks = seq(as.Date("2003-01-01"), as.Date("2012-12-31"), by = "1 year"),date_minor_breaks = "1 month" )


# geom_boxplot()
#plot(t,d,type="p",col="green3",axes=T, pch=16,cex=0.8, main="ELM_US-DK3",ylab="TG (deg C)",xaxs="i",xaxt = "n",yaxs="i",cex.lab = 1.5,cex.axis = 1.5,cex.main = 2)
#modify x-axis and y-axis intervals)
#grid(nx =11, ny = NULL,
#    lty = 2,      # Grid line type
#  col = "gray", # Grid line color
# lwd = 2)      # G
#ticks = c(365,730,1095,1460,1825,2190,2555,2921,3286,3651,4016)
#axis(side = 1, at = ticks)
#axis(side = 2)

