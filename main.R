library(readr)
library(ggplot2)

# Load Data
data <- read_csv("opsd_germany_daily.csv", show_col_types = TRUE)
head(data)
str(data)
tail(data)
dim(data)
View(data)
head(data$Date)
row.names(data$Date)

# Access Data
summary(data)
data$Consumption[1:10]
data[1:10, c("Date", "Consumption")]

# Explore: Data
str(data$Date)
exp <- data$Date
class(exp)

# Extract : Year, Month, Day
year <- format(data$Date, "%Y")
month <- format(data$Date, "%m")
day <- format(data$Date, "%d")

head(year)
head(month)
head(day)

class(year)
class(month)
class(day)

# Type Casting : Convert to Numeric
year <- as.numeric(year)
month <- as.numeric(month)
day <- as.numeric(day)

class(year)
class(month)
class(day)

# Combind Data : Year, Month, Day
data <- cbind(data, Day = day, Month = month, Year = year)
head(data)
data[1:3, c("Date", "Day", "Month", "Year")]
head(sample(data, 2))
head(sample(data, 2, 1))
head(sample(data, 1, 2))
head(sample(data, 2, 3))
head(sample(data, 3, 2))

# Visulize Data : Consumption over Time
plot(
  data$Date,
  data$Consumption,
  xlab = "Date",
  ylab = "Consumption (GWh)",
  main = "Daily Electricity Consumption in Germany"
)

plot(
  data$Year,
  data$Consumption,
  xlab = "Date",
  ylab = "Consumption (GWh)",
  ylim = c(800, 1700),
  xlim = c(2006, 2018)
)

par(mfrow = c(1, 1))

plot(data[, 2])

plot(data[, 2],
  type = "l",
  col = "blue",
  lwd = 2,
  xlab = "Time",
  ylab = "Consumption (GWh)",
  main = "Electricity Consumption Over Time"
)

plot(
  data[, 2],
  xlab = "year",
  ylab = "Consumption (GWh)",
  type = "l",
  lwd = 2,
  col = "blue",
  main = "Electricity Consumption in Germany"
)

plot(
  data[, 2],
  xlab = "year",
  ylab = "Consumption (GWh)",
  type = "l",
  lwd = 2,
  col = "blue",
  xlim = c(0, 2018),
  main = "Electricity Consumption in Germany"
)

plot(
  data[, 2],
  xlab = "year",
  ylab = "Consumption (GWh)",
  type = "l",
  lwd = 2,
  col = "blue",
  xlim = c(2006, 2018),
  main = "Electricity Consumption in Germany"
)

plot(
  data[, 2],
  xlab = "year",
  ylab = "Consumption (GWh)",
  type = "l",
  lwd = 2,
  col = "blue",
  xlim = c(2006, 2018),
  ylim = c(1000, 1400),
  main = "Electricity Consumption in Germany"
)

# log value of Consumption and defferences of logs

plot(
  10 * diff(log(data[, 2])),
  xlab = "Daily",
  ylab = "Consumption (GWh)",
  type = "l",
  lwd = 2,
  col = "orange",
  ylim = c(-3.5, 3.5),
  main = "Daily Consumption in: log(C1), log(C2), ..., log(Cn)"
)

# In GGPLOT

ggplot(data, type = "o") +
  geom_line(aes(x = year, y = Consumption))

# Data Prepration For Consumption, Wind, Solar, Wind+Solar

# Consumption
head(data[, 2], na.rm = TRUE)
tail(data[, 2], na.rm = TRUE)
min(data[, 2], na.rm = TRUE)
max(data[, 2], na.rm = TRUE)

# Wind
head(data$Wind)
min(data$Wind, na.rm = TRUE)
max(data$Wind, na.rm = TRUE)

# Solar
head(data$Solar)
tail(data$Solar)
min(data$Solar, na.rm = TRUE)
max(data$Solar, na.rm = TRUE)

# Wind + Solar
head(data[, 5], na.rm = TRUE)
tail(data[, 5], na.rm = TRUE)
min(data[, 5], na.rm = TRUE)
max(data[, 5], na.rm = TRUE)

# Multi Plot 3 X 1
par(mfrow = c(2, 1))
consumption_plot <- plot(
  data[, 2],
  xlab = "Date",
  ylab = "(GWh)",
  main = "Electricity Power Generation",
  type = "l",
  col = "orange"
)

x_ray_consumption_plot <- plot(
  data$Date,
  data$Consumption,
  xlab = "Date",
  ylab = "(GWh)",
  main = "Electricit Power Generationny x ray",
  type = "l",
  ylim = c(800, 1800)
)

wind_plot <- plot(
  data$Date,
  data$Wind,
  xlab = "Date",
  ylab = "(GWh)",
  main = "Wind Power Generation",
  type = "l",
  col = "green"
)
solar_plot <- plot(
  data$Date,
  data$Solar,
  xlab = "Date",
  ylab = "(GWh)",
  main = "Solar Power Generation",
  type = "l",
  col = "orange"
)
wind_solar_plot <- plot(
  data$Date,
  data[, 5],
  xlab = "Date",
  ylab = "(GWh)",
  main = "Wind & Solar Power Generation",
  type = "l",
  col = "purple"
)
