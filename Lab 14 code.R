install.packages("gridExtra")
library(gridExtra)
library(ggplot2)
# downloading necessary tool :(
setwd("C:/Users/jacks/OneDrive/Documents/AGR 333 Labs/Lab 14") #setting working directory :/

#I don't want to add comments this whole time. if the code works, it works.

WASDE <- read.csv("WASDE.csv")
head(WASDE)
str(WASDE)

#^^ getting the csv into the environment and looking at it... how fun and InTeReStInG

#Now were gonna make graphs and other visual representations of data. mY fAvOrItE :/

g_price <- ggplot(data = WASDE, aes(x = year, y = corn_price)) +
  geom_line(color = "darkviolet") +
  ggtitle("Corn Price Over Time") +
  labs(y = "Corn Price ($)", x = "Time (years)")

#the figure for corn price... yippie.....

g_demand <- ggplot(data = WASDE, aes(x = year, y = all_domestic_use)) +
  geom_line(color = "orange") +
  ggtitle("Corn Demand Over Time") +
  labs(y = "Corn Demand", x = "Time (years)")

#Graph for demand. the data set does not have a specific demand for corn, so I used the general domestic use statistic.

g_supply <- ggplot(data = WASDE, aes(x = year, y = total_supply)) +
  geom_line(color = "lightblue3") +
  ggtitle("Corn Supply Over Time") +
  labs(y = "Corn Supply", x = "Time (years)")

#Now we put them into a grid arrangement thingy... i guess...

grid.arrange(g_price, g_demand, g_supply, nrow=3)

#it works... splendid day...

#now things start to get trickier. you know, when i first started this class I didn't know why no-one wants to be a data scientist anymore. now I get it. this is not fun, and is actually dreadful.

WASDE$SUR <- (WASDE$total_supply - WASDE$all_domestic_use)/(WASDE$all_domestic_use)

ggplot(data = WASDE, aes(x = SUR, y = corn_price)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm, color = "red4") +
  ggtitle("Corn Prices v.s. Stock-to-Use Ratio") +
  labs(y = "Corn Price ($)", x = "Stock-to-Use Ratio")

#why do the 1 and the l look so similar in R... that feels like cruel and unsual punishment.

reg1 <- lm(corn_price ~ SUR, data = WASDE)
summary(reg1)

#coolio, now we've got the funky numbers to do number things!

install.packages("gtsummary")
library(gtsummary)
library(broom.helpers)
tbl_regression(reg1, intercept = TRUE) %>%
  add_glance_source_note(include = c(r.squared, nobs))

#Now we gotta get the averages! *does a backflip*

mean_sur <- mean(WASDE$SUR)
mean_price <- mean(WASDE$corn_price)

#now we can get ready to make a histogram of the stuffs

summary(resid(reg1))

hist(resid(reg1), 
     main = "Histogram of Linear Regression Errors",
     xlab = "Linear Model Residuals")

#Now we gotta make another scatter plot... womp womp :(


ggplot(data = WASDE, aes(x = SUR, y = resid(reg1))) +
  geom_point(shape = 1) +
  ggtitle("Linear Regression Errors vs. Stock-to-Use Ratio") +
  labs(y = "Errors", x = "Stock-to-Use Ratio")

#ok, now onto more, worse, less fun things.

WASDE$SUR_Inv <- 1 / WASDE$SUR
reg2 <- lm(corn_price ~ SUR_Inv, data=WASDE)

summary(reg2)

hist(resid(reg2), main="Histogram of Non-linear Regression Errors", xlab="Non-linear Model Residuals")

ggplot(data=WASDE, aes(x=SUR, y=resid(reg2))) +
  geom_point(shape=1) +
  ggtitle("Non-linear Regression Errors vs. Stock-to-Use Ratio") +
  labs(y="Errors", x="Stock-to-Use Ratio")

#now we gotta do this scary sounding thing called "Time-Split Analysis"... sounds sketchy...

WASDE$period <- ifelse(WASDE$year >= 2006, "2006-2019", "1973-2005")
WASDE$P2006 <- as.numeric(WASDE$year >= 2006)

ggplot(data=WASDE, aes(x=SUR, y=corn_price, color=period)) +
  geom_point(shape=1) +
  geom_smooth(method=lm, se=FALSE) +
  ggtitle("Corn Prices vs. Stock-to-Use Ratio (1973–2019)") +
  labs(y="Corn Price ($)", x="Stock-to-Use Ratio")

reg3 <- lm(corn_price ~ SUR + P2006 + SUR:P2006, data=WASDE)

summary(reg3)

#I just opened the Auto-Correlation Check and wanted to throw up... why does it look like that... stressful

error <- ts(resid(reg3), start=1973, end=2019, frequency=1)
lag_error <- lag(error, -1)
error <- cbind(error, lag_error)

reg4 <- lm(error ~ lag_error, data=error)

summary(reg4)

