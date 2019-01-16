#
#
#
# Arab Forecasting Testing
# 18 Dec

# Read in the Arab data

#import data
pacman::p_load(readxl,
               caret,
               leaps,
               tidyverse,
               dplyr,
               olsrr,
               nnet,
               devtools)

suppressWarnings(expr)
Arab_final <- readxl::read_excel("Imputed Complete Long Data/Arab_final_expanded.xlsx")

Arab.cols <- c("index","Year","Mobile.Cell.Subs","Population.Density","Percent.Border.Conflict",
               "Government.1","Government.3","Government.4","Government.5", "Fertility.Rate",
               "Trade.percent.GDP", "Two.Year.Conflict.Intensity.Trend", "HIIK.Conflict.Intensity")

Arab_forecast <- Arab_final %>% dplyr::select_(.dots = Arab.cols)
#write.csv(Arab_forecast, file = "Arab_forecest_original_subset.csv")
# Fix Factor Variables

Arab_forecast$Government.1 <- as.factor(Arab_forecast$Government.1)
Arab_forecast$Government.3 <- as.factor(Arab_forecast$Government.3)
Arab_forecast$Government.4 <- as.factor(Arab_forecast$Government.4)
Arab_forecast$Government.5 <- as.factor(Arab_forecast$Government.5)
Arab_forecast$HIIK.Conflict.Intensity <- as.factor(Arab_forecast$HIIK.Conflict.Intensity)

# Build Regression Models 

attach(Arab_forecast)
Arab_Mobile.Cell.Subs_reg <- lm(log(Mobile.Cell.Subs)~HIIK.Conflict.Intensity+Population.Density+Government.1+Fertility.Rate+Percent.Border.Conflict+Trade.percent.GDP+Two.Year.Conflict.Intensity.Trend+Government.5+Government.3, data = Arab_forecast)
Arab_Mobile.Cell.Subs_residuals <- as.vector(Arab_Mobile.Cell.Subs_reg$residuals)
#
Arab_Population.Density_reg <- lm((Population.Density^(1/2))~Fertility.Rate+Trade.percent.GDP+Percent.Border.Conflict+Mobile.Cell.Subs+HIIK.Conflict.Intensity+Government.1+Government.3+Government.4+Government.5+Two.Year.Conflict.Intensity.Trend+(Fertility.Rate*Trade.percent.GDP)+(Fertility.Rate*Percent.Border.Conflict)+(Fertility.Rate*HIIK.Conflict.Intensity)+(Fertility.Rate*Government.3)+(Trade.percent.GDP*Percent.Border.Conflict)+(Trade.percent.GDP*Mobile.Cell.Subs)+(Trade.percent.GDP*HIIK.Conflict.Intensity)+(Trade.percent.GDP*Government.1)+(Trade.percent.GDP*Government.3)+(Trade.percent.GDP*Government.4)+(Trade.percent.GDP*Government.5)+(Trade.percent.GDP*Two.Year.Conflict.Intensity.Trend)+(Percent.Border.Conflict*Mobile.Cell.Subs)+(Percent.Border.Conflict*HIIK.Conflict.Intensity)+(Percent.Border.Conflict*Government.1)+(Percent.Border.Conflict*Government.4)+(Percent.Border.Conflict*Two.Year.Conflict.Intensity.Trend)+(Mobile.Cell.Subs*HIIK.Conflict.Intensity)+(Mobile.Cell.Subs*Government.4)+(HIIK.Conflict.Intensity*Government.1)+(HIIK.Conflict.Intensity*Two.Year.Conflict.Intensity.Trend)+(Government.1*Two.Year.Conflict.Intensity.Trend)+(Government.5*Two.Year.Conflict.Intensity.Trend),data = Arab_forecast)
Arab_Population.Density_residuals <- as.vector(Arab_Population.Density_reg$residuals)
#
Arab_Percent.Border.Conflict_reg <- lm(Percent.Border.Conflict~Fertility.Rate+Population.Density+Trade.percent.GDP+Mobile.Cell.Subs+HIIK.Conflict.Intensity+Government.1+Government.3+Government.4+Government.5+Two.Year.Conflict.Intensity.Trend+(Fertility.Rate*Population.Density)+(Fertility.Rate*Trade.percent.GDP)+(Fertility.Rate*Mobile.Cell.Subs)+(Fertility.Rate*HIIK.Conflict.Intensity)+(Fertility.Rate*Government.3)+(Population.Density*Mobile.Cell.Subs)+(Population.Density*HIIK.Conflict.Intensity)+(Population.Density*Government.1)+(Population.Density*Government.4)+(Trade.percent.GDP*Mobile.Cell.Subs)+(Trade.percent.GDP*HIIK.Conflict.Intensity)+(Trade.percent.GDP*Government.1)+(Trade.percent.GDP*Government.4)+(Mobile.Cell.Subs*HIIK.Conflict.Intensity)+(Mobile.Cell.Subs*Government.1)+(HIIK.Conflict.Intensity*Government.1)+(HIIK.Conflict.Intensity*Government.3)+(HIIK.Conflict.Intensity*Government.4)+(HIIK.Conflict.Intensity*Government.5)+(HIIK.Conflict.Intensity*Two.Year.Conflict.Intensity.Trend), data = Arab_forecast)
Arab_Percent.Border.Conflict_residuals <- as.vector(Arab_Percent.Border.Conflict_reg$residuals)
#
Arab_Fertility.Rate_reg <- lm(Fertility.Rate~Population.Density+Trade.percent.GDP+Percent.Border.Conflict+Mobile.Cell.Subs+HIIK.Conflict.Intensity+Government.1+Government.3+Government.4+Government.5+Two.Year.Conflict.Intensity.Trend+(Population.Density*Trade.percent.GDP)+(Population.Density*Percent.Border.Conflict)+(Population.Density*Mobile.Cell.Subs)+(Population.Density*HIIK.Conflict.Intensity)+(Population.Density*Government.3)+(Population.Density*Government.4)+(Population.Density*Government.5)+(Population.Density*Two.Year.Conflict.Intensity.Trend)+(Trade.percent.GDP*Percent.Border.Conflict)+(Trade.percent.GDP*Mobile.Cell.Subs)+(Trade.percent.GDP*HIIK.Conflict.Intensity)+(Trade.percent.GDP*Government.1)+(Percent.Border.Conflict*Mobile.Cell.Subs)+(Percent.Border.Conflict*HIIK.Conflict.Intensity)+(Percent.Border.Conflict*Government.1)+(Percent.Border.Conflict*Government.5)+(Mobile.Cell.Subs*HIIK.Conflict.Intensity)+(Mobile.Cell.Subs*Government.1)+(HIIK.Conflict.Intensity*Government.1), data = Arab_forecast)
Arab_Fertility.Rate_residuals <- as.vector(Arab_Fertility.Rate_reg$residuals)
#
Arab_Trade.Percent.GDP_reg <- lm(Trade.percent.GDP~Fertility.Rate+Population.Density+Percent.Border.Conflict+Mobile.Cell.Subs+HIIK.Conflict.Intensity+Government.1+Government.3+Government.4+Government.5+Two.Year.Conflict.Intensity.Trend+(Fertility.Rate*Population.Density)+(Fertility.Rate*Percent.Border.Conflict)+(Fertility.Rate*Government.1)+(Population.Density*Mobile.Cell.Subs)+(Population.Density*Government.1)+(Population.Density*Government.3)+(Population.Density*Government.4)+(Population.Density*Two.Year.Conflict.Intensity.Trend)+(Percent.Border.Conflict*Mobile.Cell.Subs)+(Percent.Border.Conflict*HIIK.Conflict.Intensity)+(Percent.Border.Conflict*Government.1)+(Percent.Border.Conflict*Government.4)+(Percent.Border.Conflict*Two.Year.Conflict.Intensity.Trend)+(Mobile.Cell.Subs*Government.1)+(Mobile.Cell.Subs*Government.3)+(Mobile.Cell.Subs*Government.5)+(HIIK.Conflict.Intensity*Government.1)+(Government.1*Two.Year.Conflict.Intensity.Trend), data = Arab_forecast)
Arab_Trade.Percent.GDP_residuals <- as.vector(Arab_Trade.Percent.GDP_reg$residuals)
detach(Arab_forecast)
# Generate Conflict Status Binary Var
Conflict_Status <- ifelse(as.numeric(as.character(Arab_forecast$HIIK.Conflict.Intensity)) >= 3, 1, 0)
Arab_forecast$Conflict_Status <- Conflict_Status
#
# Define conversion equation from log odds to probability, also other relevant equations 
logit2prob <- function(logit){
  odds <- exp(logit) 
  if (is.infinite(odds)) {
    prob <- 1
  } else {
  prob <- odds / (1 + odds)
  }
  return(prob)
}
# Define Built in and out of Conflcit models that will depend on previous years conflict status 
in_conflict <- function(data_year) {
  attach(data_year)
  intercept <- -5.56
  in_Mobile.Cell.Subs <- -0.000
  in_Population.Density <- -0.0304
  in_Percent.Border.Conflict <- 2.5943
  in_Government.1 <-  -1.4569
  in_Government.3 <- -20.0807
  in_Government.4 <- -3.5082
  in_Government.5 <- -5.4795
  in_conflict_trans <- intercept + (in_Mobile.Cell.Subs * data_year$Mobile.Cell.Subs) +
    (in_Population.Density * data_year$Population.Density) +
  (in_Percent.Border.Conflict * data_year$Percent.Border.Conflict) + (in_Government.1 * as.numeric(as.character(data_year$Government.1))) +
  (in_Government.3 * as.numeric(as.character(data_year$Government.3))) + (in_Government.4 * as.numeric(as.character(data_year$Government.4))) + 
    (in_Government.5 * as.numeric(as.character(data_year$Government.5)))
  logit_trans <- logit2prob(in_conflict_trans)
  return(logit_trans)
  detach(data_year)
}
#
out_conflict <-  function(data_year) {
  attach(data_year)
  intercept <- -0.8759
  out_Fertility.Rate <- 2.2693
  out_Trade.percent.GDP <- -0.0978
  out_Two.Year.Conflict.Intensity.Trend <- 17.3657
  out_conflict_trans <- intercept + (out_Fertility.Rate * Fertility.Rate) + 
    (out_Trade.percent.GDP * Trade.percent.GDP) + (out_Two.Year.Conflict.Intensity.Trend * 
                                                     Two.Year.Conflict.Intensity.Trend)
  logit_trans <- logit2prob(out_conflict_trans)
  return(logit_trans)
  detach(data_year)
}

#
rbind.all.columns <- function(x, y) {
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  x[, c(as.character(y.diff))] <- NA
  y[, c(as.character(x.diff))] <- NA
  return(rbind(x, y))
}
#
# Operational value ranges
Mobile.Cell.Subs_max <- (2*max(Arab_forecast$Mobile.Cell.Subs))
Mobile.Cell.Subs_min <- (.5*min(Arab_forecast$Mobile.Cell.Subs))
Population.Density_max <- (2*max(Arab_forecast$Population.Density))
Population.Density_min <- (.5*min(Arab_forecast$Population.Density))
Percent.Border.Conflict_max <- (2*max(Arab_forecast$Percent.Border.Conflict)) #No min necessary, it can be zero
Fertility.Rate_max <- (2*max(Arab_forecast$Fertility.Rate))
Fertility.Rate_min <- (.5*min(Arab_forecast$Fertility.Rate))
Trade.percent.GDP_max <- 100
Trade.percent.GDP_min <- (.5*min(Arab_forecast$Trade.percent.GDP))

# FOR LOOP SECTION
Arab_forecast_1 <- Arab_forecast[1:187,]
Arab_forecast_1$index
table(Arab_forecast_1$index)
#country_ids <- c(3,12,51,78,84,88,92,95,110,121,130,138,157,165,170,179,180)
country_ids <- c(180)
forecast_list = Arab_forecast_1 %>% filter(index == 179 & Year==2014)
#overall_list = list()

for (reps in 1:5) {
  Arab_forecast_1 <- Arab_forecast[1:187,]
  for (country in country_ids) {
    Arab_forecast_1 <- Arab_forecast[1:187,] #test test test 
      for (n in 2015:2030) {
  # Arab_forecast_1 must be amended each time
  # Variable one year prediction for index 3 with regression equations and noise (normal)
      # Beginning with 2014 date and then increasing each year by n
prev_year <- (n-1)
prev_prev_year <- (n-2)
sim <- reps
Arab_forecast_1 <- Arab_forecast_1 %>% filter(Year >= prev_prev_year) %>% 
  filter(index == country) %>% mutate(Rep = sim)
#Arab_forecast_1 <- Arab_forecast_1 %>% filter(Year >= prev_year) %>% mutate(Rep = sim)
subset_Arab <- Arab_forecast_1 %>% filter(Year == prev_year) %>% filter(index == country) %>% filter(Rep == sim)

# in conflict   
new_Mobile.Cell.Subs <- exp(predict(Arab_Mobile.Cell.Subs_reg, subset_Arab))
new_Population.Density <- (predict(Arab_Population.Density_reg, subset_Arab)^2)
new_Percent.Border.Conflict <- predict(Arab_Percent.Border.Conflict_reg, subset_Arab)
# out of conflict 
new_Fertility.Rate <- predict(Arab_Fertility.Rate_reg, subset_Arab)
new_Trade.Percent.GDP <- predict(Arab_Trade.Percent.GDP_reg, subset_Arab)
# Variables remaining constant or calculated 
new_Government.1 <- as.numeric(as.character(subset_Arab$Government.1))
new_Government.3 <- as.numeric(as.character(subset_Arab$Government.3))
new_Government.4 <- as.numeric(as.character(subset_Arab$Government.4))
new_Government.5 <- as.numeric(as.character(subset_Arab$Government.5))

subset_Arab_prev_year <- Arab_forecast_1 %>% filter(Year == prev_prev_year) %>%
  filter(index == country) %>% filter(Rep == sim)
new_Two.Year.Conflict.Intensity.Trend <- ((as.numeric(as.character(subset_Arab$HIIK.Conflict.Intensity))-
                                            as.numeric(as.character(subset_Arab_prev_year$HIIK.Conflict.Intensity))) / 6)

# Add Noise - No variables have negative minimum values just to note
Arab_country <- Arab_forecast_1 %>% filter(index == country) #Should this only be from the observed data
Arab_country
#
# If mean is 0 then possibility of huge negative values that for example made new Mobile noise negative
#
new_Mobile.Cell.Subs_noise <- as.numeric(new_Mobile.Cell.Subs) + sample(Arab_Mobile.Cell.Subs_residuals, size = 1)

new_Population.Density_noise <- as.numeric(new_Population.Density) + sample(Arab_Population.Density_residuals, size = 1)

new_Percent.Border.Conflict_noise <- as.numeric(new_Percent.Border.Conflict) + sample(Arab_Percent.Border.Conflict_residuals, size = 1)

new_Fertility.Rate_noise <- as.numeric(new_Fertility.Rate) + sample(Arab_Fertility.Rate_residuals, size = 1)

new_Trade.Percent.GDP_noise <- as.numeric(new_Trade.Percent.GDP) + sample(Arab_Trade.Percent.GDP_residuals, size = 1)

# Enforce non-negativity constraints 
vars_values <- c(new_Mobile.Cell.Subs_noise, new_Population.Density_noise, new_Percent.Border.Conflict_noise,
                 new_Fertility.Rate_noise, new_Trade.Percent.GDP_noise)
vars_names <- c("new_Percent.Border.Conflict_noise")
# Non Negativity 
for (i in 1:length(vars_values)) {
  if (vars_values[i] <= 0 | is.na(vars_values[i])) {
    vars_values[i] <- 0
    assign(vars_names[i],vars_values[i])
  }
} 
# Mobile Restrictions 
if (new_Mobile.Cell.Subs_noise > Mobile.Cell.Subs_max) {
  new_Mobile.Cell.Subs_noise <- Mobile.Cell.Subs_max
} else if (new_Mobile.Cell.Subs_noise < Mobile.Cell.Subs_min) {
  new_Mobile.Cell.Subs_noise <- Mobile.Cell.Subs_min
}
# Population Density Restrictions
if (new_Population.Density_noise > Population.Density_max) {
  new_Population.Density_noise <- Population.Density_max
} else if (new_Population.Density_noise < Population.Density_min) {
  new_Population.Density_noise <- Population.Density_min
}
# Percent Border Conflict 
if (new_Percent.Border.Conflict_noise > Percent.Border.Conflict_max) {
  new_Percent.Border.Conflict_noise <- Percent.Border.Conflict_max
} else if (new_Percent.Border.Conflict_noise < 0) {
  new_Percent.Border.Conflict_noise <- 0
}

# Fertility Rate Restrictions
if (new_Fertility.Rate_noise > Fertility.Rate_max) {
  new_Fertility.Rate_noise <- Fertility.Rate_max
} else if (new_Fertility.Rate_noise < Fertility.Rate_min) {
  new_Fertility.Rate_noise <- Fertility.Rate_min
}
# Trade.percent.GDP Restrictions
if (new_Trade.Percent.GDP_noise > Trade.percent.GDP_max) {
  new_Trade.Percent.GDP_noise <- Trade.percent.GDP_max
} else if (new_Trade.Percent.GDP_noise < Trade.percent.GDP_min) {
  new_Trade.Percent.GDP_noise <- Trade.percent.GDP_min
}

# First add new row for next year 
yearr <- n
countryy <- country
new_row <- data.frame(countryy, yearr, new_Mobile.Cell.Subs_noise, new_Population.Density_noise, new_Percent.Border.Conflict_noise,
                      new_Government.1, new_Government.3, new_Government.4, new_Government.5, 
                      new_Fertility.Rate_noise, new_Trade.Percent.GDP_noise,new_Two.Year.Conflict.Intensity.Trend, sim)
names(new_row) <- c(names(Arab_country)[1:12], 'Rep')
Arab_country <- rbind.all.columns(Arab_country, new_row)
Arab_country[which(Arab_country$Year==2014),'Rep'] <- sim
# Use Neumann Models with new data and compare transition probability to 
last_year <- Arab_country %>%
  filter(Year == (n-1) & Rep == sim)

this_year <- Arab_country %>%
  filter(Year == n & Rep == sim)

if (last_year$Conflict_Status == 1) {
    transition_probability <- in_conflict(this_year)
    # See if it will actually transition or remain the same with a 0.5 cutoff
    if (transition_probability > 0.5) { #Higher chance of exiting conflict
      random_draw <- runif(1)
      if (random_draw > transition_probability) { #Then it will not transition even though high prob it will
        Arab_country <- Arab_country %>% mutate(Conflict_Status=replace(Conflict_Status, Arab_country$Year == n, 1))
        # Map HIIK Accordingly - Since it was supposed to transition, make lowest HIIK still in conflict
        Arab_country <- Arab_country %>% mutate(HIIK.Conflict.Intensity=replace(HIIK.Conflict.Intensity,
                                                                                Arab_country$Year == n, 3))
      } else if (random_draw < transition_probability) { #Do transition
        Arab_country <- Arab_country %>% mutate(Conflict_Status=replace(Conflict_Status, Arab_country$Year == n, 0))
        # Map HIIK According to transition probability
        if (transition_probability > (5/6)) {
          Arab_country <- Arab_country %>% mutate(HIIK.Conflict.Intensity=replace(HIIK.Conflict.Intensity,
                                                                                  Arab_country$Year == n, 0))
        } else if (transition_probability <= (5/6) & transition_probability > (4/6)) {
          Arab_country <- Arab_country %>% mutate(HIIK.Conflict.Intensity=replace(HIIK.Conflict.Intensity,
                                                                                  Arab_country$Year == n, 1))
        } else if (transition_probability <= (4/6) & transition_probability > (3/6)) {
          Arab_country <- Arab_country %>% mutate(HIIK.Conflict.Intensity=replace(HIIK.Conflict.Intensity,
                                                                                  Arab_country$Year == n, 2))
        }
      }
    } else if (transition_probability < 0.5) { #Higher probability of remaining in conflict
      random_draw <- runif(1)
      if (random_draw < transition_probability) { #Then doesn't remain the same even though it should, exit conflict
        Arab_country <- Arab_country %>% mutate(Conflict_Status=replace(Conflict_Status, Arab_country$Year == n, 0))
        # Maps HIIK to highest out of conflict - 2
        Arab_country <- Arab_country %>% mutate(HIIK.Conflict.Intensity=replace(HIIK.Conflict.Intensity,
                                                                                Arab_country$Year == n, 2))
      } else if (random_draw > transition_probability) { #Does remain in conflict
        Arab_country <- Arab_country %>% mutate(Conflict_Status=replace(Conflict_Status, Arab_country$Year == n, 1))
        # Maps HIIK accordingly, smallest being the most in conflict HIIK - 5
        if (transition_probability <= (3/6) & transition_probability > (2/6)) {
          Arab_country <- Arab_country %>% mutate(HIIK.Conflict.Intensity=replace(HIIK.Conflict.Intensity,
                                                                                  Arab_country$Year == n, 3))
        } else if (transition_probability <= (2/6) & transition_probability > (1/6)) {
          Arab_country <- Arab_country %>% mutate(HIIK.Conflict.Intensity=replace(HIIK.Conflict.Intensity,
                                                                                  Arab_country$Year == n, 4))
        } else if (transition_probability <= (1/6) & transition_probability >= 0) {
          Arab_country <- Arab_country %>% mutate(HIIK.Conflict.Intensity=replace(HIIK.Conflict.Intensity,
                                                                                  Arab_country$Year == n, 5))
          }
        }
    }
} else if (last_year$Conflict_Status == 0) {
    transition_probability <- out_conflict(this_year)
    # See if it will actually transition
    if (transition_probability > 0.5) { #Examine higher probability to transition into conflict
      random_draw <- runif(1)
      if (random_draw > transition_probability) { #Then it will remain out of conflict even though it has high prob to transition
        Arab_country <- Arab_country %>% mutate(Conflict_Status=replace(Conflict_Status, Arab_country$Year == n, 0))
        # HIIK becomes highest out of conflict value
        Arab_country <- Arab_country %>% mutate(HIIK.Conflict.Intensity=replace(HIIK.Conflict.Intensity,
                                                                                Arab_country$Year == n, 2))
      } else if (random_draw < transition_probability) { #Then it will enter conflict
        Arab_country <- Arab_country %>% mutate(Conflict_Status=replace(Conflict_Status, Arab_country$Year == n, 1))
        # HIIK maps according to transition probability
        if (transition_probability > (5/6)) {
          Arab_country <- Arab_country %>% mutate(HIIK.Conflict.Intensity=replace(HIIK.Conflict.Intensity,
                                                                                  Arab_country$Year == n, 5))
        } else if (transition_probability <= (5/6) & transition_probability > (4/6)) {
          Arab_country <- Arab_country %>% mutate(HIIK.Conflict.Intensity=replace(HIIK.Conflict.Intensity,
                                                                                  Arab_country$Year == n, 4))
        } else if (transition_probability <= (4/6) & transition_probability > (3/6)) {
          Arab_country <- Arab_country %>% mutate(HIIK.Conflict.Intensity=replace(HIIK.Conflict.Intensity,
                                                                                  Arab_country$Year == n, 3))
        }
      }
    } else if (transition_probability < 0.5) { #Wants to remain out of conflict
      random_draw <- runif(1)
      if (random_draw < transition_probability) { # Then it will transition even though higher probability to remain out of conflict
        Arab_country <- Arab_country %>% mutate(Conflict_Status=replace(Conflict_Status, Arab_country$Year == n, 1))
        # HIIK becomes lowest in conflict value - 3
        Arab_country <- Arab_country %>% mutate(HIIK.Conflict.Intensity=replace(HIIK.Conflict.Intensity,
                                                                                Arab_country$Year == n, 3))
      } else if (random_draw > transition_probability) { #Wants to remain the same and will
        Arab_country <- Arab_country %>% mutate(Conflict_Status=replace(Conflict_Status, Arab_country$Year == n, 0))
        # Map HIIK based on transition probability
        if (transition_probability <= (3/6) & transition_probability > (2/6)) {
          Arab_country <- Arab_country %>% mutate(HIIK.Conflict.Intensity=replace(HIIK.Conflict.Intensity,
                                                                                  Arab_country$Year == n, 2))
        } else if (transition_probability <= (2/6) & transition_probability > (1/6)) {
          Arab_country <- Arab_country %>% mutate(HIIK.Conflict.Intensity=replace(HIIK.Conflict.Intensity,
                                                                                  Arab_country$Year == n, 1))
        } else if (transition_probability <= (1/6) & transition_probability >= 0) {
          Arab_country <- Arab_country %>% mutate(HIIK.Conflict.Intensity=replace(HIIK.Conflict.Intensity,
                                                                                  Arab_country$Year == n, 0))
        }
      }
    }
}

forecast <- as.data.frame(Arab_country[nrow(Arab_country),])

# Add new year's row to overall forecasted list
Arab_forecast_1 <- rbind.all.columns(Arab_forecast_1, forecast)
#

forecast <- forecast %>% 
  mutate(Rep = reps) %>%
  filter(Year == n & Rep == reps) %>% 
  mutate(transition.prob = transition_probability) %>% 
  mutate(random.draw = random_draw)

forecast_list <- rbind.all.columns(forecast_list, forecast)
# End of actual code
#
#forecast_list[[reps]] <- forecast[reps]
#print(transition_probability)

    }
  }
}

View(Arab_country)


sub_15<- Arab_forecast_1 %>% filter(Year==2015)
sub_15

View(Arab_forecast_1 %>%
       arrange(index))
forecast_list <- (forecast_list %>%
       arrange(index))
View(forecast_list)

View(forecast_list %>%
       arrange(index) %>%
       select(index, Year, HIIK.Conflict.Intensity, Conflict_Status, transition.prob, random.draw, Rep))

write.csv(forecast_list, file = "C:/Users/ZKane/OneDrive/Documents/KaneThesis/Forecasting/Arab results/Original/Arab_180_repstofive_thirtyyears_4jan.csv")
