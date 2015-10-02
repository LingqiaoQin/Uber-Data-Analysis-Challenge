#######  JSON dataset of client logins from an Uber city  ######

install.packages("rjson")   # to read the data from the JSON file
install.packages("lubridate")  # Date Time functions
install.packages("quantmod")   # timeseries analysis
install.packages("forecast")   # timeseries forecasting
install.packages("chron")    # create chronological object
install.packages("zoo")    # created ordered observations

library(rjson)
library(lubridate)
library(quantmod)
library(forecast)
library(xts)
library(chron)
library(zoo)

json_file = 'logins.json'  # this file stored in the local directory

json_data = fromJSON(paste(readLines(json_file), collapse=""))  # read the JSON file

logins.data.frame = data.frame(json_data)  # creating the dataframe

colnames(logins.data.frame) = c('DateTime')

names(logins.data.frame)

dim(logins.data.frame)  # 22447 rows and 1 column

print(head(logins.data.frame))  # the below is the snapshot of the raw data frame 

#  DateTime
#1 2012-03-01T00:05:55+00:00
#2 2012-03-01T00:06:23+00:00
#3 2012-03-01T00:06:52+00:00
#4 2012-03-01T00:11:23+00:00
#5 2012-03-01T00:12:47+00:00
#6 2012-03-01T00:12:54+00:00

logins.data.frame$DateTime = ymd_hms(logins.data.frame$DateTime)  # convert to datatime

print(head(logins.data.frame))

#  DateTime
#1 2012-03-01 00:05:55
#2 2012-03-01 00:06:23
#3 2012-03-01 00:06:52
#4 2012-03-01 00:11:23
#5 2012-03-01 00:12:47
#6 2012-03-01 00:12:54

range(logins.data.frame$DateTime)

# "2012-03-01 00:05:55 UTC" "2012-04-30 23:59:29 UTC"
# i.e. 1st March 2012 to 30 th April 2012 , i.e. 2 months of user logins data


# Extracting the features from the login datetime

logins.data.frame$Year = year(logins.data.frame$DateTime)  # extract year
logins.data.frame$Month = month(logins.data.frame$DateTime) # extract month
logins.data.frame$Day = mday(logins.data.frame$DateTime)  # extract day
logins.data.frame$Hour = hour(logins.data.frame$DateTime)  # extract hour

print(head(logins.data.frame))

#  DateTime            Year Month Day Hour
#1 2012-03-01 00:05:55 2012     3   1    0
#2 2012-03-01 00:06:23 2012     3   1    0
#3 2012-03-01 00:06:52 2012     3   1    0
#4 2012-03-01 00:11:23 2012     3   1    0
#5 2012-03-01 00:12:47 2012     3   1    0
#6 2012-03-01 00:12:54 2012     3   1    0


# Extract the unique values and storing in a vector
year.vector = unique(logins.data.frame$Year)
month.vector = unique(logins.data.frame$Month)
day.vector = unique(logins.data.frame$Day)
hour.vector = unique(logins.data.frame$Hour)

output.dataFrame = data.frame()  # creating an output data frame

for (i in year.vector)
{
  for (j in month.vector)
  {
    for (k in day.vector)
    {
      for (l in hour.vector)
      {
        # num of logins given year, month, day and hour of the day
        NumOfLogins = nrow(logins.data.frame[logins.data.frame$Year == i 
                                             & logins.data.frame$Month == j
                                             & logins.data.frame$Day == k
                                             & logins.data.frame$Hour == l,])
        t1 = paste(i, j, k, sep = "/")
        t2 = paste(t1, l, sep = " ")
        t3 = paste(t2, ':0', sep = "")
        dTime = as.POSIXct(t3, '%Y/%m/%d %H:%M:%S')
        day_of_week = wday(dTime)
        day_of_week_Name = factor(day_of_week,
                             levels = c(1:7), 
                             labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        countLogin = data.frame(dTime, NumOfLogins, i, j, k, l, day_of_week, day_of_week_Name)
        colnames(countLogin) = c('DateTime', 'TotalLogins', 'Year', 'Month', 'Day', 'Hour', 'WeekDay', 'WeekDayName')
        output.dataFrame = rbind(output.dataFrame, countLogin)
        
      }
    }
  }
}

print(head(output.dataFrame))


#  DateTime            TotalLogins Year Month Day Hour WeekDay WeekDayName
#1 2012-03-01 00:00:00          31 2012     3   1    0       5    Thursday
#2 2012-03-01 01:00:00          18 2012     3   1    1       5    Thursday
#3 2012-03-01 02:00:00          37 2012     3   1    2       5    Thursday
#4 2012-03-01 03:00:00          23 2012     3   1    3       5    Thursday
#5 2012-03-01 04:00:00          14 2012     3   1    4       5    Thursday
#6 2012-03-01 05:00:00           8 2012     3   1    5       5    Thursday

########################## Total Number of Logins grouped per month #########################

tot.logins.month = numeric(12)

for (i in 1:12)
{
  month.login = output.dataFrame[output.dataFrame$Month == i,]
  tot.logins.month[i] = with(month.login, sum(month.login$TotalLogins))
}

print(tot.logins.month)

# [1]     0     0 10131 12316     0     0     0     0     0     0     0     0

plot(seq(1, 12), y = tot.logins.month,
     xlab = "Months in a year", ylab = "Number of logins",
     main = "Monthly distribution of Uber logins", type = "l", cex = 0.5, lwd = 2)


######################## Total Number of Logins grouped per Day of the Month #####################

tot.logins.day = numeric(31)

for (i in 1:31)
{
  day.login = output.dataFrame[output.dataFrame$Day == i,]
  tot.logins.day[i] = with(day.login, sum(day.login$TotalLogins))
}

print(tot.logins.day)

#[1]  844  573  747  720  474  566  711  760  556  683  766  549  582  831  858  545  757  828  618  687  979 1081
#[23]  724  817  882  648  548  970  949  616  578

plot(seq(1, 31), y = tot.logins.day,
     xlab = "Days", ylab = "Number of logins",
     main = "Daily distribution of Uber logins", type = "l", cex = 0.5, lwd = 2)

######################## Total Number of Logins grouped per Days of Week ##########################

tot.logins.week = numeric(7)  # On Week basis

for (i in 1:7)
{
  week.login = output.dataFrame[output.dataFrame$WeekDay == i,]
  tot.logins.week[i] = with(week.login, sum(week.login$TotalLogins))
}

print(tot.logins.week)

# [1] 5173 2139 1861 2155 2857 3198 5064

plot(seq(1, 7), y = tot.logins.week,
     xlab = "Weekdays", ylab = "Number of logins",
     main = "Distribution of Uber logins by Days of Week", type = "l", cex = 0.5, lwd = 2)

################### Total Number of Logins grouped per hour of the day ##########################

tot.logins.hour = numeric(24)  # On Hour Basis

for (i in 1:24)
{
  hour.login = output.dataFrame[output.dataFrame$Hour == i - 1,]
  tot.logins.hour[i] = with(hour.login, sum(hour.login$TotalLogins)) 
}

print(tot.logins.hour)

#[1] 1667 1711 1911 1661 1235  926  671  378  263  247  325  471  628  702  678  708  691  762  795  813  883 1119
#[23] 1473 1729

plot(seq(1, 24), y = tot.logins.hour,
     xlab = "Hours of the Day", ylab = "Number of logins",
     main = "Distribution of Uber logins by Hour of the Day", type = "l", cex = 0.5, lwd = 2)



######################### Time Series Analysis of Logins grouped per hour of the day #########################

Hourly.ts.dataframe = output.dataFrame[c("DateTime", "TotalLogins")]

any(is.na(Hourly.ts.dataframe))   # False


Hourly.ts =  zoo(Hourly.ts.dataframe[-1], as.chron(format(Hourly.ts.dataframe$DateTime)))
                                      

Hourly.ts[1:5]

#(03/01/12 00:00:00) (03/01/12 01:00:00) (03/01/12 02:00:00) (03/01/12 03:00:00) (03/01/12 04:00:00) 
# 31                  18                  37                  23                  14

acf(Hourly.ts)
pacf(Hourly.ts)


Hourly.auto.arima.fit = auto.arima(Hourly.ts, d = NA, D = NA, max.p = 3,max.q = 3,
                               max.P = 2, max.Q = 2, max.order = 3, start.p = 2, start.q = 2,
                               start.P = 1, start.Q = 1, stationary = FALSE, seasonal = TRUE,
                               ic = c("aic"), stepwise = TRUE, trace = FALSE,
                               approximation = FALSE, xreg = NULL,
                               test = c("kpss", "adf", "pp"), seasonal.test = c("ocsb", "ch"),
                               allowdrift = FALSE, lambda = NULL, parallel = FALSE, num.cores = NULL)

print(summary(Hourly.auto.arima.fit))

#Seasonal ARIMA model
#Series: Hourly.ts 
#ARIMA(1,1,0)(1,0,0)[24]                    

#Coefficients:
#  ar1   sar1
#-0.1866  0.263
#s.e.   0.0276  0.027

#sigma^2 estimated as 48.15:  log likelihood=-4910.86
#AIC=9827.73   AICc=9827.74   BIC=9843.59
#
#Training set error measures:
#  ME     RMSE      MAE MPE MAPE      MASE         ACF1
#Training set -0.0103186 6.936712 4.992713 NaN  Inf 0.6196782 -0.001654817
#ME     RMSE      MAE MPE MAPE      MASE         ACF1
#Training set -0.0103186 6.936712 4.992713 NaN  Inf 0.6196782 -0.001654817

Hourly.forecast.OneDay = forecast(Hourly.auto.arima.fit, h = 24, level = c(90), fan = FALSE, xreg = NULL, bootstrap = FALSE)

#           Point Forecast  Lo 90     Hi 90
#15461.00       15.81756   4.4037872 27.23134
#15461.04       14.96449   0.2517874 29.67718
#15461.08       13.13552  -4.4767139 30.74776
#15461.12       14.44824  -5.6138569 34.51033
#15461.17       11.02979 -11.2199374 33.27952
#15461.21       11.81868 -12.4210298 36.05840
#15461.25       10.76674 -15.3117208 36.84520
#15461.29       10.50375 -17.2920581 38.29956
#15461.33       10.24076 -19.1722972 39.65382
#15461.38       11.02973 -19.9161753 41.97563
#15461.42       10.24076 -22.1655654 42.64708
#15461.46       10.76674 -23.0369730 44.57045
#15461.50       12.34467 -22.8009073 47.49026
#15461.54       12.34467 -24.0933951 48.78274
#15461.58       12.87065 -24.8156032 50.55691
#15461.62       12.34467 -26.5497327 51.23908
#15461.67       12.87065 -27.1954911 52.93680
#15461.71       11.29272 -29.9118566 52.49729
#15461.75       11.55571 -30.7566782 53.86809
#15461.79       11.81870 -31.5732262 55.21062
#15461.83       13.13364 -31.3116037 57.57889
#15461.88       14.18560 -31.2885787 59.65978
#15461.92       13.92261 -32.5577285 60.40295
#15461.96       13.92261 -33.5425652 61.38779


accuracy(Hourly.forecast.OneDay)

#                  ME     RMSE      MAE     MPE MAPE  MASE         ACF1
#Training set -0.0103186 6.936712 4.992713 NaN  Inf 0.6196782 -0.001654817

plot(Hourly.forecast.OneDay, ylab = "Total Number of Logins",
     xlab = "Time", las = 1, lwd = 1.5)


Hourly.forecast.short = forecast(Hourly.auto.arima.fit, h = 24*7*2, level = c(90), fan = FALSE, xreg = NULL, bootstrap = FALSE)

Hourly.forecast.long = forecast(Hourly.auto.arima.fit, h = 24*7*15, level = c(90), fan = FALSE, xreg = NULL, bootstrap = FALSE)

