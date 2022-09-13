library(lubridate)
library(nnet)
library(ggplot2)

# Import Data
circuits = read.csv('circuits.csv')
constructors = read.csv('constructors.csv')
drivers = read.csv('drivers.csv')
races = read.csv ('races.csv')
results = read.csv('results.csv')

# Merge Data into One Data Frame
df = merge(x = races, y = results, by = c("raceId")) 
df = merge(x = df, y = drivers, by = c("driverId"))
df = merge(x = df, y = circuits, by = c("circuitId"))
df = merge(x = df, y = constructors, by = c("constructorId"))

# Remove Unnecessary Columns
df = subset(df, select=-c(time.x, url.x, fp1_date, fp1_time,
                          fp2_date, fp2_time, fp3_date, fp3_time,
                          quali_date, quali_time, sprint_date, sprint_time,
                          url.y))

# Include races only since 2018
df <- df[df$year %in% df$year[df$year>2018],]

# Account for Racing Point becoming Aston Martin
# and Renault becoming Alpine 
# and Toro Rosso becoming AlphaTauri
df$constructorId[df$constructorId == 211] <- 117
df$constructorId[df$constructorId == 4] <- 214
df$constructorId[df$constructorId == 5] <- 213
df$constructorRef[df$constructorRef == "racing_point"] <- "williams"
df$constructorRef[df$constructorRef == "renault"] <- "alpine"
df$constructorRef[df$constructorRef == "toro_rosso"] <- "alphatauri"

# Check Number of Constructors (should be 10)
number_teams = unique(df$constructorRef)

# Drop former drivers (raced since cut off, but not in 2022)
# Aitken 851
# Fittipaldi 104 & 850
# Giovinazzi 841
# Grosjean 154
# Hulkenberg 807
# Kubica 9
# Kvyat 826
# Mazepin 853
# Raikkonen 8
df <- subset(df, driverId!=(851))
df <- subset(df, driverId!=(104))
df <- subset(df, driverId!=(850))
df <- subset(df, driverId!=(841))
df <- subset(df, driverId!=(154))
df <- subset(df, driverId!=(807))
df <- subset(df, driverId!=(9))
df <- subset(df, driverId!=(826))
df <- subset(df, driverId!=(853))
df <- subset(df, driverId!=(8))

# Check for 20 current drivers
numbers_drivers = unique(df$surname)

# Check for how many races
number_race = unique(df$raceId)

# Look at one example race
race_1074 = df[df$raceId %in% df$raceId[df$raceId==1074],]

# Create features for win and podium rates for each driver
df$podium <- ifelse(df$positionOrder<=3, 1, 0)
df$win <- ifelse(df$positionOrder<=1, 1, 0)

wins <- data.frame(tapply(df$win, df$surname, sum))
wins$names  <- rownames(wins)
podiums <- data.frame(tapply(df$podium, df$surname, sum))
podiums$names <- rownames(podiums)
race_count <- data.frame(tapply(df$raceId, df$surname, length))
race_count$names <- rownames(race_count)

records = merge(x = wins, y = podiums, by = c("names")) 
records = merge(x = records, y = race_count, by = c("names"))
colnames(records) = c("surname", "wins", "podiums", "races")

records$winrate = records$wins/records$races
records$podiumrate = records$podiums/records$races

driver_records <- data.frame(records)

# Create features for Constructor success
constructorRef <- c("mercedes", "red_bull", "ferrari", "mclaren",
                "alpine", "alphatauri", "aston_martin", "williams",
                "alfa", "haas")
team_rank_2021 <- c(1,2,3,4,5,6,7,8,9,10)
team_points_2021 <- c(613.5, 585.5, 323.5, 275, 155,
                      142, 77, 23, 13, 0)

teams_2021 <- data.frame(constructorRef, team_rank_2021, team_points_2021)

df = merge(x = df, y = teams_2021, by = c("constructorRef"))
df = merge(x = df, y = driver_records, by = c("surname"))


# Create feature for driver age at race date
as.Date(df$dob)
as.Date(df$date)
df$driver_age = trunc((df$dob %--% df$date) / years(1))

# Create a table matching current drivers and constuctors 
current_drivers = data.frame(sort(driver_records$surname))
current_teams = c("williams", "alpine", "alfa", "alphatauri", "mercedes",
                  "williams", "ferrari", "haas", "mclaren", "alpine",
                  "red_bull", "mclaren", "mercedes", "ferrari", "haas",
                  "aston_martin", "alphatauri", "red_bull", "aston_martin", "alfa")

driver_teams = cbind(current_drivers, current_teams)
colnames(driver_teams) = c("surname", "constructorRef")

driver_teams = merge(x = driver_teams, y = teams_2021, by = c("constructorRef"))



# Multinomial Logistic Regression Model
# y = binary 1/0 win/loss 
# 1. Regress dependent variable (y) on independent variables (features)
mlogit <- multinom(win ~ winrate + podiumrate + team_points_2021, data = df)

# 2. Record linear regression coefficients including intercept (betas)
intercept <- coef(mlogit)[1]

b_winrate <- coef(mlogit)[2]

b_podiumrate <- coef(mlogit)[3]

b_team_point_2021 <- coef(mlogit)[4]

# 3. Put the coefficients (betas) into a table with the independent variables (X's)

output = subset(driver_records, select=-c(wins, podiums, races))

output = merge(x = output, y = driver_teams, by = c("surname"))

# output$b_winrate = b_winrate

output$b_winrate = b_winrate

output$b_podiumrate = b_podiumrate

output$b_teampoints = b_team_point_2021

output$intercept = intercept

# 4. Calculate dependent variable y using betas and X's (logit estimates)

output$logit = output$intercept +
                  output$b_winrate * output$winrate +
                  output$b_podiumrate * output$podiumrate +
                  output$b_teampoints * output$team_points_2021

# 5. Convert logit estimates to win probabilities by driver

output$n = exp(output$logit)

output$sum_n = sum(output$n)

output$prob = output$n / output$sum_n

output = subset(output, select=c(surname, prob))

# Finally, make a nice chart

output[11,1] = "Perez"

plot = ggplot(data=output, aes(x=reorder(surname, -prob), y=prob)) +
  geom_bar(stat="identity", fill="blue1") +
  xlab("Driver") +
  ylab("Next Race Win Probability") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme(panel.background = element_blank())

plot

#  Testing

mlogit2 <- multinom(win ~ podiumrate + team_points_2021 + driver_age, data = df)

intercept_2 <- coef(mlogit2)[1]

b_podiumrate_2 <- coef(mlogit2)[2]

b_team_point_2021_2 <- coef(mlogit2)[3]

b_driver_age <- coef(mlogit2)[4]

# Manually enter ages

driver_records$age = c(26, 41, 33, 26, 37, 
                       27, 24, 29, 22, 25,
                       32, 33, 24, 28, 23,
                       23, 22, 24, 35, 22)

output2 = subset(driver_records, select=-c(wins, podiums, races, winrate))

output2 = merge(x = output2, y = driver_teams, by = c("surname"))

# output$b_winrate = b_winrate

output2$b_podiumrate = b_podiumrate

output2$b_teampoints = b_team_point_2021

output2$b_driver_age = b_driver_age

output2$intercept = intercept

output2$logit = output2$intercept +
  output2$b_podiumrate * output2$podiumrate +
  output2$b_teampoints * output2$team_points_2021 +
  output2$b_driver_age * output2$age

output2$n = exp(output2$logit)

output2$sum_n = sum(output2$n)

output2$prob = output2$n / output2$sum_n

output2 = subset(output2, select=c(surname, prob))

# Finally, make a nice chart

output2[11,1] = "Perez"

plot = ggplot(data=output2, aes(x=reorder(surname, -prob), y=prob)) +
  geom_bar(stat="identity", fill="blue1") +
  xlab("Driver") +
  ylab("Next Race Win Probability") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme(panel.background = element_blank())

plot