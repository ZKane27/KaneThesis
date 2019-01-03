#
#
#
#
# SE Asia Forecasting Testing
# 23 Dec

# Read in the SE Asia data

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
SE_Asia_final <- readxl::read_excel("Imputed Complete Long Data/SE_Asia_final_expanded.xlsx")

SE_Asia.cols <- c("index","Year","Internet.Users","Life.Expectancy","Mobile.Cell.Subs",
                  "Infant.Mortality.Rate",
                  "Population.Growth","Arable.Lands","Average.Border.Conflict","Binary.Border.Conflict",
                  "Freshwater.per.Capita","Two.Year.Conflict.Intensity.Trend","HIIK.Conflict.Intensity",
                  "Military.Expend.GDP","Regime.Type.Democratic","Regime.Type.Emerging.Transitional",
                  "Trade.percent.GDP","Freedom.Score","GDP.Per.Capita","Population.Density")

SE_Asia_forecast <- SE_Asia_final %>% dplyr::select_(.dots = SE_Asia.cols)

# Fix Factor Variables

SE_Asia_forecast$Regime.Type.Democratic <- as.factor(SE_Asia_forecast$Regime.Type.Democratic)
SE_Asia_forecast$Regime.Type.Emerging.Transitional <- as.factor(SE_Asia_forecast$Regime.Type.Emerging.Transitional)
SE_Asia_forecast$HIIK.Conflict.Intensity <- as.factor(SE_Asia_forecast$HIIK.Conflict.Intensity)
SE_Asia_forecast$Binary.Border.Conflict <- as.factor(SE_Asia_forecast$Binary.Border.Conflict)

# Build Regression Models 

attach(SE_Asia_forecast)
SE_Asia_Internet.Users_reg <- lm(Internet.Users~GDP.Per.Capita+Trade.percent.GDP+Mobile.Cell.Subs+Population.Density+Infant.Mortality.Rate+Arable.Lands+Military.Expend.GDP+HIIK.Conflict.Intensity+Freedom.Score+Average.Border.Conflict+Regime.Type.Democratic+Life.Expectancy+Regime.Type.Emerging.Transitional+Binary.Border.Conflict+Freshwater.per.Capita, data = SE_Asia_forecast)
SE_Asia_Internet.Users_residuals <- as.vector(SE_Asia_Internet.Users_reg$residuals)
#
SE_Asia_Life.Expectancy_reg <- lm(Life.Expectancy~Infant.Mortality.Rate+Freshwater.per.Capita+Regime.Type.Emerging.Transitional+HIIK.Conflict.Intensity+Freedom.Score+Internet.Users+Regime.Type.Democratic+GDP.Per.Capita+Average.Border.Conflict+Population.Density+Military.Expend.GDP+Binary.Border.Conflict+Trade.percent.GDP+Arable.Lands, data = SE_Asia_forecast)

#
SE_Asia_Mobile.Cell.Subs_reg <- lm((Mobile.Cell.Subs^(1/4))~GDP.Per.Capita+Population.Density+HIIK.Conflict.Intensity+Internet.Users+Trade.percent.GDP+Binary.Border.Conflict+Life.Expectancy+Average.Border.Conflict+Regime.Type.Emerging.Transitional+Freedom.Score+Regime.Type.Democratic, data = SE_Asia_forecast)
#
SE_Asia_Infant.Mortality.Rate_reg <- lm(Infant.Mortality.Rate~Life.Expectancy+Freshwater.per.Capita+Trade.percent.GDP+Binary.Border.Conflict+Internet.Users+Freedom.Score+HIIK.Conflict.Intensity+GDP.Per.Capita+Population.Growth+Arable.Lands+Military.Expend.GDP+Mobile.Cell.Subs+Regime.Type.Emerging.Transitional, data = SE_Asia_forecast)
#
SE_Asia_Population.Growth_reg <- lm(Population.Growth~Internet.Users+Life.Expectancy+Mobile.Cell.Subs+Infant.Mortality.Rate+Arable.Lands+Average.Border.Conflict+Binary.Border.Conflict+Freshwater.per.Capita+Two.Year.Conflict.Intensity.Trend+HIIK.Conflict.Intensity+Military.Expend.GDP+Regime.Type.Democratic+Regime.Type.Emerging.Transitional+Trade.percent.GDP+Freedom.Score+GDP.Per.Capita+Population.Density+(Internet.Users*Life.Expectancy)+(Internet.Users*Infant.Mortality.Rate)+(Internet.Users*Arable.Lands)+(Internet.Users*Average.Border.Conflict)+(Internet.Users*Two.Year.Conflict.Intensity.Trend)+(Internet.Users*HIIK.Conflict.Intensity)+(Internet.Users*Military.Expend.GDP)+(Internet.Users*Military.Expend.GDP)+(Internet.Users*Regime.Type.Democratic)+(Internet.Users*Trade.percent.GDP)+(Internet.Users*Population.Density)+(Life.Expectancy*Infant.Mortality.Rate)+(Life.Expectancy*Arable.Lands)+(Life.Expectancy*Average.Border.Conflict)+(Life.Expectancy*Binary.Border.Conflict)+(Life.Expectancy*Freshwater.per.Capita)+(Life.Expectancy*HIIK.Conflict.Intensity)+(Life.Expectancy*Military.Expend.GDP)+(Life.Expectancy*Regime.Type.Democratic)+(Life.Expectancy*Trade.percent.GDP)+(Life.Expectancy*GDP.Per.Capita)+(Life.Expectancy*Population.Density)+(Mobile.Cell.Subs*Average.Border.Conflict)+(Mobile.Cell.Subs*Binary.Border.Conflict)+(Mobile.Cell.Subs*HIIK.Conflict.Intensity)+(Mobile.Cell.Subs*Freedom.Score)+(Infant.Mortality.Rate*Arable.Lands)+(Infant.Mortality.Rate*Binary.Border.Conflict)+(Infant.Mortality.Rate*Two.Year.Conflict.Intensity.Trend)+(Infant.Mortality.Rate*HIIK.Conflict.Intensity)+(Infant.Mortality.Rate*Military.Expend.GDP)+(Infant.Mortality.Rate*Regime.Type.Democratic)+(Infant.Mortality.Rate*Regime.Type.Emerging.Transitional)+(Infant.Mortality.Rate*Trade.percent.GDP)+(Infant.Mortality.Rate*GDP.Per.Capita)+(Infant.Mortality.Rate*Population.Density)+(Arable.Lands*Binary.Border.Conflict)+(Arable.Lands*Two.Year.Conflict.Intensity.Trend)+(Arable.Lands*HIIK.Conflict.Intensity)+(Arable.Lands*Military.Expend.GDP)+(Arable.Lands*Regime.Type.Democratic)+(Arable.Lands*Freedom.Score)+(Arable.Lands*GDP.Per.Capita)+(Average.Border.Conflict*HIIK.Conflict.Intensity)+(Average.Border.Conflict*Regime.Type.Democratic)+(Average.Border.Conflict*Regime.Type.Emerging.Transitional)+(Average.Border.Conflict*GDP.Per.Capita)+(Binary.Border.Conflict*Freshwater.per.Capita)+(Binary.Border.Conflict*Two.Year.Conflict.Intensity.Trend)+(Binary.Border.Conflict*HIIK.Conflict.Intensity)+(Binary.Border.Conflict*Military.Expend.GDP)+(Binary.Border.Conflict*Regime.Type.Democratic)+(Binary.Border.Conflict*Trade.percent.GDP)+(Binary.Border.Conflict*Population.Density)+(Freshwater.per.Capita*HIIK.Conflict.Intensity)+(Freshwater.per.Capita*Military.Expend.GDP)+(Freshwater.per.Capita*Regime.Type.Democratic)+(Freshwater.per.Capita*Trade.percent.GDP)+(Freshwater.per.Capita*Freedom.Score)+(Freshwater.per.Capita*GDP.Per.Capita)+(Freshwater.per.Capita*Population.Density)+(Two.Year.Conflict.Intensity.Trend*HIIK.Conflict.Intensity)+(Two.Year.Conflict.Intensity.Trend*Regime.Type.Democratic)+(Two.Year.Conflict.Intensity.Trend*Regime.Type.Emerging.Transitional)+(Two.Year.Conflict.Intensity.Trend*Population.Density)+(HIIK.Conflict.Intensity*Military.Expend.GDP)+(HIIK.Conflict.Intensity*Regime.Type.Democratic)+(HIIK.Conflict.Intensity*Trade.percent.GDP)+(HIIK.Conflict.Intensity*Freedom.Score)+(HIIK.Conflict.Intensity*GDP.Per.Capita)+(HIIK.Conflict.Intensity*Population.Density)+(Military.Expend.GDP*Regime.Type.Democratic)+(Military.Expend.GDP*Regime.Type.Emerging.Transitional)+(Military.Expend.GDP*Trade.percent.GDP)+(Military.Expend.GDP*Freedom.Score)+(Military.Expend.GDP*Population.Density)+(Regime.Type.Democratic*Freedom.Score)+(Regime.Type.Democratic*GDP.Per.Capita)+(Regime.Type.Emerging.Transitional*Trade.percent.GDP)+(Regime.Type.Emerging.Transitional*GDP.Per.Capita)+(Trade.percent.GDP*Freedom.Score)+(Trade.percent.GDP*GDP.Per.Capita)+(Trade.percent.GDP*Population.Density)+(GDP.Per.Capita*Population.Density), data = SE_Asia_forecast)
#
SE_Asia_Arable.Lands_reg <- lm(log(Arable.Lands)~Average.Border.Conflict+Trade.percent.GDP+HIIK.Conflict.Intensity+Internet.Users+Population.Growth+Population.Density+Military.Expend.GDP+Infant.Mortality.Rate+Regime.Type.Emerging.Transitional+Life.Expectancy+Freedom.Score, data = SE_Asia_forecast)
#
SE_Asia_Average.Border.Conflict_reg <- lm(Average.Border.Conflict~Binary.Border.Conflict+Population.Density+GDP.Per.Capita+Life.Expectancy+Trade.percent.GDP+Internet.Users+Mobile.Cell.Subs+Arable.Lands+HIIK.Conflict.Intensity+Freshwater.per.Capita+Military.Expend.GDP+Two.Year.Conflict.Intensity.Trend+Population.Growth, SE_Asia_forecast)
#
SE_Asia_Binary.Border.Conflict_reg <- glm(Binary.Border.Conflict ~ Internet.Users+Life.Expectancy+
                                            Mobile.Cell.Subs+Infant.Mortality.Rate+Population.Growth+
                                            Arable.Lands+Average.Border.Conflict+Freshwater.per.Capita+
                                            Two.Year.Conflict.Intensity.Trend+HIIK.Conflict.Intensity+Military.Expend.GDP+
                                            Regime.Type.Democratic+Regime.Type.Emerging.Transitional+
                                            Freedom.Score+GDP.Per.Capita+Population.Density+
                                            Average.Border.Conflict,family=binomial(link='logit'),data=SE_Asia_forecast)
#
SE_Asia_Freshwater.Per.Capita_reg <- lm(log(Freshwater.per.Capita)~Life.Expectancy+Population.Density+Infant.Mortality.Rate+GDP.Per.Capita+Population.Growth+HIIK.Conflict.Intensity+Military.Expend.GDP+Arable.Lands+Average.Border.Conflict+Freedom.Score+Regime.Type.Emerging.Transitional+Trade.percent.GDP+Regime.Type.Democratic+Binary.Border.Conflict+Internet.Users+Mobile.Cell.Subs, data = SE_Asia_forecast)
#
SE_Asia_Military.Expend.GDP_reg <- lm(Military.Expend.GDP~Regime.Type.Emerging.Transitional+HIIK.Conflict.Intensity+Trade.percent.GDP+GDP.Per.Capita+Arable.Lands+Internet.Users+Mobile.Cell.Subs+Freshwater.per.Capita+Regime.Type.Democratic+Population.Density+Population.Growth+Average.Border.Conflict+Life.Expectancy+Infant.Mortality.Rate, data = SE_Asia_forecast )
#
SE_Asia_GDP.Per.Capita_reg <- lm(GDP.Per.Capita~Population.Density+Internet.Users+Trade.percent.GDP+Mobile.Cell.Subs+Binary.Border.Conflict+Average.Border.Conflict+HIIK.Conflict.Intensity+Military.Expend.GDP+Freshwater.per.Capita+Life.Expectancy+Infant.Mortality.Rate+Population.Growth+Two.Year.Conflict.Intensity.Trend, data = SE_Asia_forecast)
#
SE_Asia_Population.Density_reg <- lm(Population.Density~GDP.Per.Capita+Trade.percent.GDP+Average.Border.Conflict+Regime.Type.Democratic+HIIK.Conflict.Intensity+Mobile.Cell.Subs+Internet.Users+Regime.Type.Emerging.Transitional+Freedom.Score+Life.Expectancy+Arable.Lands+Military.Expend.GDP+Freshwater.per.Capita+Population.Growth+Two.Year.Conflict.Intensity.Trend, data = SE_Asia_forecast)
#
SE_Asia_Trade.percent.GDP_reg <- lm(Trade.percent.GDP~Population.Density+Internet.Users+GDP.Per.Capita+HIIK.Conflict.Intensity+Population.Growth+Arable.Lands+Military.Expend.GDP+Mobile.Cell.Subs+Infant.Mortality.Rate+Average.Border.Conflict+Freedom.Score+Regime.Type.Emerging.Transitional+Regime.Type.Democratic+Life.Expectancy+Two.Year.Conflict.Intensity.Trend, data = SE_Asia_forecast)
#
SE_Asia_Freedom.Score_reg <- lm(Freedom.Score~HIIK.Conflict.Intensity+Regime.Type.Democratic+Regime.Type.Emerging.Transitional+Life.Expectancy+Internet.Users+Population.Density+Trade.percent.GDP+Binary.Border.Conflict+Infant.Mortality.Rate+Population.Growth+Freshwater.per.Capita, data = SE_Asia_forecast)
#
detach(SE_Asia_forecast)
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
  intercept <- -79.0581
  in_Internet.Users <- -0.0676
  in_Life.Expectancy <- 1.0474
  in_Mobile.Cell.Subs <- -0.000
  in_Infant.Mortality.Rate <- 0.089
  in_Population.Growth <- 1.966
  in_Arable.Lands <- 3.788
  in_Average.Border.Conflcit <- -2.1159
  in_Binary.Border.Conflict <- 4.9459
  in_Freshwater.per.Capita <- 0.0001
  in_Two.Year.Conflict.Intensity.Trend <- -10.296
  in_Military.Expend.GDP <- 0.8437
  in_Regime.Type.Democratic <- -1.3044
  in_Regime.Type.Emerging.Transitional <- -15.7118
  in_GDP.Per.Capita <- -0.0001
  in_conflict_trans <- intercept + (in_Internet.Users * data_year$Internet.Users) + (in_Life.Expectancy * data_year$Life.Expectancy) + 
    (in_Mobile.Cell.Subs * data_year$Mobile.Cell.Subs) + (in_Infant.Mortality.Rate * data_year$Infant.Mortality.Rate) + 
    (in_Population.Growth * data_year$Population.Growth) + (in_Arable.Lands * data_year$Arable.Lands) + 
    (in_Average.Border.Conflict * data_year$Average.Border.Conflict) + (in_Binary.Border.Conflict * data_year$Binary.Border.Conflict) + 
    (in_Freshwater.per.Capita * data_year$Freshwater.per.Capita) + (in_Two.Year.Conflict.Intensity.Trend * data_year$Two.Year.Conflict.Intensity.Trend) + 
    (in_Military.Expend.GDP * data_year$Military.Expend.GDP) + (in_Regime.Type.Democratic * as.numeric(as.character(data_year$Regime.Type.Democratic))) + 
    (in_Regime.Type.Emerging.Transitional * as.numeric(as.character(data_year$Regime.Type.Emerging.Transitional))) + 
    (in_GDP.Per.Capita * data_year$GDP.Per.Capita)
  logit_trans <- logit2prob(in_conflict_trans)
  return(logit_trans)
  detach(data_year)
}
# as.numeric(as.character(data_year$Government.5))
#predict <- ifelse(predict(logisticmodel, type="response")>.5, 1, 0)
#
#
out_conflict <-  function(data_year) {
  attach(data_year)
  intercept <- 5.3436
  out_Mobile.Cell.Subs <- 0.0000
  out_Trade.percent.GDP <- -0.0632
  out_Binary.Border.Conflict <- 5.9973
  out_Two.Year.Conflict.Intensity.Trend <- 16.514
  out_Freedom.Score <- -9.8479
  out_Regime.Type.Democratic <- 3.9917
  out_Average.Border.Conflict <- -1.5551
  out_GDP.Per.Capita <- -0.0001
  out_Population.Density <- 0.0026
  out_conflict_trans <- intercept + (out_Mobile.Cell.Subs * Mobile.Cell.Subs) + 
    (out_Trade.percent.GDP * Trade.percent.GDP) + (out_Binary.Border.Conflict * Binary.Border.Conflict) + 
    (out_Two.Year.Conflict.Intensity.Trend * Two.Year.Conflict.Intensity.Trend) + 
    (out_Freedom.Score * Freedom.Score) + (out_Regime.Type.Democratic * Regime.Type.Democratic) + 
    (out_Average.Border.Conflict * Average.Border.Conflict) + (out_GDP.Per.Capita * GDP.Per.Capita) + 
    (out_Population.Density * Population.Density)
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

# Operational value ranges
Internet.Users_max <- (2*max(SE_Asia_forecast$Internet.Users)) # can be zero 
Life.Expectancy_max <- (2*max(SE_Asia_forecast$Life.Expectancy))
Life.Expectancy_min <- (.5*min(SE_Asia_forecast$Life.Expectancy))
Mobile.Cell.Subs_max <- (2*max(SE_Asia_forecast$Mobile.Cell.Subs)) # can be zero
Infant.Mortality.Rate_max <- (2*max(SE_Asia_forecast$Infant.Mortality.Rate))
Infant.Mortality.Rate_min <- (.5*min(SE_Asia_forecast$Infant.Mortality.Rate))
Population.Growth_max <- (2*max(SE_Asia_forecast$Population.Growth))
Population.Growth_min <- (2*min(SE_Asia_forecast$Population.Growth)) #times 2 because negative min
Arable.Lands_max <- (2*max(SE_Asia_forecast$Arable.Lands))
Arable.Lands_min <- (.5*min(SE_Asia_forecast$Arable.Lands))
Average.Border.Conflict_max <- (2*max(SE_Asia_forecast$Average.Border.Conflict)) # can be zero
Freshwater.per.Capita_max <- (2*max(SE_Asia_forecast$Freshwater.per.Capita))
Freshwater.per.Capita_min <- (.5*min(SE_Asia_forecast$Freshwater.per.Capita))
Military.Expend.GDP_max <- (2*max(SE_Asia_forecast$Military.Expend.GDP))
Military.Expend.GDP_min <- (.5*min(SE_Asia_forecast$Military.Expend.GDP))
Population.Density_max <- (2*max(SE_Asia_forecast$Population.Density))
Population.Density_min <- (.5*min(SE_Asia_forecast$Population.Density))
Percent.Border.Conflict_max <- (2*max(SE_Asia_forecast$Percent.Border.Conflict)) #No min necessary, it can be zero
Trade.percent.GDP_max <- max(SE_Asia_forecast$Trade.percent.GDP)
Trade.percent.GDP_min <- (.5*min(SE_Asia_forecast$Trade.percent.GDP))
Freedom.Score_max <- 1 # maps between 0 and 1
GDP.Per.Capita_max <- (2*max(SE_Asia_forecast$GDP.Per.Capita))
GDP.Per.Capita_min <- (.5*min(SE_Asia_forecast$GDP.Per.Capita))



# FOR LOOP SECTION
View(SE_Asia_forecast)
SE_Asia_forecast_1 <- SE_Asia_forecast[1:308,]
SE_Asia_forecast_1$index
table(SE_Asia_forecast_1$index)
#country_ids <- c(3,12,51,78,84,88,92,95,110,121,130,138,157,165,170,179,180)
country_ids <- c(13)
forecast_list = SE_Asia_forecast_1 %>% filter(index <= 13 & Year==2014)
#overall_list = list()


for (reps in 1) {
  SE_Asia_forecast_1 <- SE_Asia_forecast[1:308,]
  for (country in country_ids) {
    SE_Asia_forecast_1 <- SE_Asia_forecast[1:308,] #test test test 
    for (n in 2015:2016) {
      # SE_Asia_forecast_1 <- SE_Asia_forecast[1:308,] must be amended each time
      # Variable one year prediction for index 13 with regression equations and noise (normal)
      # Beginning with 2014 date and then increasing each year by n
      prev_year <- (n-1)
      prev_prev_year <- (n-2)
      sim <- reps
      SE_Asia_forecast_1 <- SE_Asia_forecast_1 %>% filter(Year >= prev_prev_year) %>% 
        filter(index == country) %>% mutate(Rep = sim)
      #Arab_forecast_1 <- Arab_forecast_1 %>% filter(Year >= prev_year) %>% mutate(Rep = sim)
      subset_SE_Asia <- SE_Asia_forecast_1 %>% filter(Year == prev_year) %>% filter(index == country) %>% filter(Rep == sim)
      
      # in conflict
      new_Internet.Users <- predict(SE_Asia_Internet.Users_reg, subset_SE_Asia)
      new_Life.Expectancy <- predict(SE_Asia_Life.Expectancy_reg, subset_SE_Asia)
      new_Mobile.Cell.Subs <- (predict(SE_Asia_Mobile.Cell.Subs_reg, subset_SE_Asia)^4)
      new_Infant.Mortality.Rate <- predict(SE_Asia_Infant.Mortality.Rate_reg, subset_SE_Asia)
      new_Population.Growth <- predict(SE_Asia_Population.Growth_reg, subset_SE_Asia)
      new_Arable.Lands <- exp(predict(SE_Asia_Arable.Lands_reg, subset_SE_Asia))
      
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
      vars_names <- c("new_Mobile.Cell.Subs_noise", "new_Population.Density_noise", "new_Percent.Border.Conflict_noise",
                      "new_Fertility.Rate_noise", "new_Trade.Percent.GDP_noise")
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
      data.frame(countryy,yearr,new_Mobile.Cell.Subs_noise,new_Population.Density_noise, new_Percent.Border.Conflict_noise,
                 new_Government.1,new_Government.3,new_Government.4,new_Government.5,
                 new_Fertility.Rate_noise,new_Trade.Percent.GDP_noise,new_Two.Year.Conflict.Intensity.Trend)
      new_Two.Year.Conflict.Intensity.Trend
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






