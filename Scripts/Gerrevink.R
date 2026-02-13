###Recreating Gerrevink's carbon emissions proportion numbers 

##library
library(ggplot2)
library(tidyverse)
##Load extracted dataset 

options(scipen =999)

Gerrevink_organic <- read.csv("Data/Gerrevink_organic.csv")


#plot data 

ggplot(Gerrevink_organic, aes(x = Year_since_fire, y = percent_carbon_respired))+
  geom_point()






fit <- nls(percent_carbon_respired ~ a * exp(-b * Year_since_fire), data = Gerrevink_organic,
           start = list(a = 6, b = 0.1))


summary(fit)

# New x values
new_x <- data.frame(Year_since_fire = 0:70)

# Predict y values using the model
predicted_y <- predict(fit, newdata = new_x)

prediction_df <- data.frame(
  Year_since_fire = new_x$Year_since_fire,
  predicted_percent = predicted_y
)


valid_19 <- prediction_df%>%
  filter(Year_since_fire < 20)

valid_19$cumulative_percent <-  cumsum(valid_19$predicted_percent)

