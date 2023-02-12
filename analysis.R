library(readr)
library(tidyverse) 
library(ggalt)
library(countrycode) 
library(rworldmap) 
library(gridExtra) 
library(broom) 
library(plotly)
library(dplyr)
library(kableExtra)
library(knitr)
library(gifski)
library(ggplot2)
library(gganimate)
library(caret)
library(ranger)
library(randomForest)
library(Metrics)
library(corrplot)
library(ggcorrplot)

theme_set(theme_light())

data <- read_csv("master.csv")

#rename columns
data <- data %>%
  select(-c('HDI for year','country-year')) %>%
  rename(gdp_for_year = 'gdp_for_year ($)', 
         gdp_per_capita = 'gdp_per_capita ($)',
         suicide_per_100k = 'suicides/100k pop') %>%
  as.data.frame()

#excluding countries with <= 3 years of data:
minimum_years <- data %>%
  group_by(country) %>%
  summarize(rows = n(), 
            years = rows / 12) %>%
  arrange(years)

data <- data %>%
  filter(!(country %in% head(minimum_years$country, 7)))


# add continent
data$continent <- countrycode(sourcevar = data[, "country"],
                              origin = "country.name",
                              destination = "continent")

# move continent to the first column
data <- data %>%
  select(continent, everything())

# Nominal factors
data_nominal <- c('country', 'sex', 'continent')
data[data_nominal] <- lapply(data[data_nominal], function(x){factor(x)})

# Making age ordinal
data$age <- gsub(" years", "", data$age) 
data$age <- factor(data$age, 
                   ordered = T, 
                   levels = c("5-14",
                              "15-24", 
                              "25-34", 
                              "35-54", 
                              "55-74", 
                              "75+"))
# Making generation ordinal
data$generation <- factor(data$generation, 
                          ordered = T, 
                          levels = c("G.I. Generation", 
                                     "Silent",
                                     "Boomers", 
                                     "Generation X", 
                                     "Millenials", 
                                     "Generation Z"))

data <- as_tibble(data)

country_mean_gdp <- data %>%
  group_by(country, continent) %>%
  filter(year != 2016)  %>%
  summarize(suicides_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000, 
            gdp_per_capita = mean(gdp_per_capita))

model1 <- lm(suicides_per_100k ~ gdp_per_capita, data = country_mean_gdp)

#-remove outliers
gdp_suicide_no_outliers <- model1 %>%
  augment() %>%
  arrange(desc(.cooksd)) %>%
  filter(.cooksd < 4/nrow(.)) %>% # removes 5/93 countries
  inner_join(country_mean_gdp, by = c("suicides_per_100k", "gdp_per_capita")) %>%
  select(country, continent, gdp_per_capita, suicides_per_100k)

model2 <- lm(suicides_per_100k ~ gdp_per_capita, data = gdp_suicide_no_outliers)

summary(model2)


country_total_population <- data %>%
  group_by(country, continent) %>%
  filter(year!=2016)%>% 
  summarize(suicides_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000, total_population= sum(as.numeric(population)))
country_total_population$suicides_per_100k <- round(country_total_population$suicides_per_100k, 2)

model3 <- lm(suicides_per_100k ~ total_population, data = country_total_population)

#-remove outliers
population_suicide_no_outliers <- model3 %>%
  augment() %>%
  filter(total_population<5000000000)%>% 
  select(total_population, suicides_per_100k )

model4 <- lm(suicides_per_100k  ~ total_population, data = population_suicide_no_outliers)

summary(model4)

#-total suicide number vs total population
country_pop_num <- data %>%
  group_by(country, continent) %>%
  filter(year!=2016)%>% 
  summarize(total_suicides = (sum(as.numeric(suicides_no))), total_population= sum(as.numeric(population)))

model5 <- lm(total_suicides ~ total_population, data = country_pop_num)

#-remove outliers
population_suicide_outliers <- model5 %>%
  augment() %>%
  filter(total_population<5000000000)%>% 
  select(total_population, total_suicides )

model6 <- lm(total_suicides  ~ total_population, data = population_suicide_outliers)
summary(model6)


##Correlation
c2 <- data  %>% select(-c('continent','country','gdp_for_year')) 
cols = c("year","sex","age","generation")
c2[, cols] <- c2 %>% select(all_of(cols)) %>% lapply(as.numeric)

cor2 <- cor(c2)

corr <- round(cor(c2, use="complete.obs"), 4)
ggcorrplot(corr, lab = TRUE, colors = c("lightcoral", "white", "dodgerblue"), 
           show.legend = T, outline.color = "gray", type = "upper", hc.order = TRUE,
           tl.cex = 15, lab_size = 5, sig.level = .4) +labs(fill = "Correlation")

## Throughout The World
country <- data %>%
  group_by(country) %>%
  summarize(suicide_per_100k = sum(suicides_no) / sum(population) * 100000) 
  
  countrydata <- joinCountryData2Map(country, joinCode = "NAME", nameJoinColumn = "country")
  par(mar=c(0, 0, 0, 0)) # margins

mapCountryData(countrydata, 
               nameColumnToPlot="suicide_per_100k", 
               mapTitle="Suicide Throughtout The World",
               colourPalette =c('white','pink','red','darkred'), 
               oceanCol="lightblue", 
               missingCountryCol="grey65", 
               catMethod = "pretty")

## By continent
continent <- data %>%
  group_by(continent) %>%
  summarize(suicide_per_100k = (sum(suicides_no) / sum(population) * 100000)) %>%
  arrange(suicide_per_100k)

continent$continent <- factor(continent$continent, ordered = T, levels = continent$continent)

continent_plot <- ggplot(continent, aes(x = continent, y = suicide_per_100k, fill = continent)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Global Suicides (per 100k), by Continent",
       x = "Continent", 
       y = "Suicides per 100k", 
       fill = "Continent") +
  theme(legend.position = "none", title = element_text(size = 10)) + 
  scale_y_continuous(breaks = seq(0, 20, 1), minor_breaks = F)


continent_time <- data %>%
  group_by(year, continent) %>%
  filter(year != 2016) %>%
  summarize(suicide_per_100k = (sum(suicides_no) / sum(population) * 100000))

continent_time$continent <- factor(continent_time$continent, ordered = T, levels = continent$continent)

continent_time_plot <- ggplot(continent_time, aes(x = year, y = suicide_per_100k, col = factor(continent))) + 
  facet_grid(continent ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "Trends Over Time, by Continent", 
       x = "Year", 
       y = "Suicides per 100k", 
       color = "Continent") + 
  theme(legend.position = "none", title = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F)

grid.arrange(continent_plot, continent_time_plot, ncol = 2)

## Over the years
global_average = sum(data$suicides_no) / sum(data$population)* 100000
data %>%
  group_by(year) %>%
  summarize(suicide_per_100k = sum(suicides_no) / sum(population) * 100000) %>%
  ggplot(aes(x = year, y = suicide_per_100k)) + 
  geom_line(aes(color = year), size = 1) + 
  geom_point(size = 2) + 
  geom_hline(yintercept = global_average, size = 1, linetype = 2, color = "grey35") +
  labs(title = "Global Suicides (per 100k)",
       subtitle = "Trend over time, 1985 - 2016.",
       x = "Year", 
       y = "Suicides per 100k") + 
  scale_x_continuous(breaks = seq(1985, 2016, 2))+
  scale_y_continuous(breaks = seq(10, 20)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## By Sex
sex_plot <- data %>%
  group_by(sex) %>%
  summarize(suicide_per_100k = sum(suicides_no) / sum(population) * 100000) %>%
  ggplot(aes(x = sex, y = suicide_per_100k, fill = sex)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Global suicides (per 100k), by Sex",
       x = "Sex", 
       y = "Suicides per 100k") +
  theme(legend.position = "none") + 
  scale_y_continuous(breaks = seq(0, 25), minor_breaks = F)

### with time
sex_time_plot <- data %>%
  group_by(year, sex) %>%
  summarize(suicide_per_100k = (sum(suicides_no) / sum(population) * 100000)) %>%
  ggplot(aes(x = year, y = suicide_per_100k, col = factor(sex))) + 
  facet_grid(sex ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "Trends Over Time, by Sex", 
       x = "Year", 
       y = "Suicides per 100k", 
       color = "Sex") + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = seq(1985, 2016, 5), minor_breaks = F)

grid.arrange(sex_plot, sex_time_plot, ncol = 2)

### with continent
ggplot(data, aes(x=continent, y=suicide_per_100k, fill=sex)) +
  geom_boxplot() +
  theme_minimal() + 
  scale_fill_brewer(palette="Set3")

## By Age
age_plot <- data %>%
  group_by(age) %>%
  summarize(suicide_per_100k = (sum(suicides_no) / sum(population) * 100000)) %>%
  ggplot(aes(x = age, y = suicide_per_100k, fill = age)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Global suicides per 100k, by Age",
       x = "Age", 
       y = "Suicides per 100k") +
  theme(legend.position = "none") + 
  scale_y_continuous(breaks = seq(0, 30, 1), minor_breaks = F)

age_time_plot <- data %>%
  group_by(year, age) %>%
  summarize(suicide_per_100k = sum(suicides_no) / sum(population) * 100000) %>%
  ggplot(aes(x = year, y = suicide_per_100k, group = age)) + 
  geom_line(aes(color = age), size = 1) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Global Suicides By Age 1985 -2016",
       x = "Year", 
       y = "Suicides per 100k") + 
  scale_x_continuous(breaks = seq(1985, 2016, 2))

grid.arrange(age_plot, age_time_plot, ncol = 2)

##Age differences, by Continent
data %>%
  group_by(continent, age) %>%
  summarize(n = n(), 
            suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  ggplot(aes(x = continent, y = suicide_per_100k, fill = age)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_hline(yintercept = global_average, linetype = 2, color = "grey35", size = 1) +
  labs(title = "Age Disparity, by Continent",
       x = "Continent", 
       y = "Suicides per 100k", 
       fill = "Age")

## By Generation
data %>%
  group_by(year, generation) %>%
  summarize(suicide_per_100k = sum(suicides_no) / sum(population) * 100000) %>%
  ggplot(aes(x = year, y = suicide_per_100k, group = generation)) + 
  geom_line(aes(color = generation), size = 1) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Global Suicides By Generation 1985 -2016",
       x = "Year", 
       y = "Suicides per 100k") + 
  scale_x_continuous(breaks = seq(1985, 2016, 2))

## By Country
country <- (head(data %>%
  group_by(country, continent) %>%
  summarize(n = n(), 
            suicide_per_100k = (sum(suicides_no) / sum(population) * 100000))%>%
  arrange(desc(suicide_per_100k)),30))

country$country <- factor(country$country, 
                          ordered = T, 
                          levels = rev(country$country))

country_plot <- ggplot(country, aes(x = country, y = suicide_per_100k, fill = continent)) + 
  geom_bar(stat = "identity") + 
  geom_hline(yintercept = global_average, linetype = 2, color = "grey35", size = 1) +
  labs(title = "Global suicides per 100k, by Country",
       x = "Country", 
       y = "Suicides per 100k", 
       fill = "Continent") + 
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 45, 2)) + 
  theme(legend.position = "bottom")

country_plot 

## Highest and lowest suicide rates
countries <- data %>% 
  group_by(country) %>% 
  summarise(mean_suicide = mean(sum(suicides_no) / sum(population) * 100000)) %>% 
  arrange(desc(mean_suicide)) %>% 
  data.frame()

d1 = head(countries, 5)
d2 = tail(countries, 5)
kable(list(d1, d2),  col.names = c("Country","Suicides Rate"), row.names = FALSE,caption = "Countries with highest and lowest suicides rates")%>%
  kable_classic(full_width = FALSE, html_font = "Cambria", font_size=14)

# Linear Trends
country_year <- data %>%
  group_by(country, year) %>%
  summarize(suicide_per_100k = sum(suicides_no) / sum(population) * 100000, 
            gdp_per_capita = mean(gdp_per_capita))

country_year_trends <- country_year %>%
  ungroup() %>%
  nest(-country) %>% # format: country, rest of data (in list column)
  mutate(model = map(data, ~ lm(suicide_per_100k ~ year, data = .)), # for each item in 'data', fit a linear model
         tidied = map(model, tidy)) %>% # tidy each of these into dataframe format - call this list 'tidied'
  unnest(tidied)

country_year_sig_trends <- country_year_trends %>%
  filter(term == "year") %>%
  mutate(p.adjusted = p.adjust(p.value, method = "holm")) %>%
  filter(p.adjusted < .05) %>%
  arrange(estimate)

country_year_sig_trends$country <- factor(country_year_sig_trends$country, 
                                          ordered = T, 
                                          levels = country_year_sig_trends$country)

ggplot(country_year_sig_trends, aes(x=country, y=estimate, col = estimate)) + 
  geom_point(stat='identity', size = 4) +
  geom_hline(yintercept = 0, col = "grey", size = 1) +
  scale_color_gradient(low = "green", high = "red") +
  geom_segment(aes(y = 0, 
                   x = country, 
                   yend = estimate, 
                   xend = country), size = 1) +
  labs(title="Change per year (Suicides per 100k)", 
       subtitle="Of countries with significant trends (p < 0.05)", 
       x = "Country", y = "Change Per Year (Suicides per 100k)") +
  scale_y_continuous(breaks = seq(-2, 2, 0.2), limits = c(-1.5, 1.5)) +
  theme(legend.position = "none") +
  coord_flip()

## Steepest increasing trends
top12_increasing <- tail(country_year_sig_trends$country, 12)

country_year %>%
  filter(country %in% top12_increasing) %>%
  ggplot(aes(x = year, y = suicide_per_100k, col = country)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~ country) + 
  theme(legend.position = "none") + 
  labs(title="12 Steepest Increasing Trends", 
       subtitle="Of countries with significant trends (p < 0.05)", 
       x = "Year", 
       y = "Suicides per 100k")

## Steepest decreasing trend
top12_decreasing <- head(country_year_sig_trends$country, 12)

country_year %>%
  filter(country %in% top12_decreasing) %>%
  ggplot(aes(x = year, y = suicide_per_100k, col = country)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~ country) + 
  theme(legend.position = "none") + 
  labs(title="12 Steepest Decreasing Trends", 
       subtitle="Of countries with significant trends (p < 0.05)", 
       x = "Year", 
       y = "Suicides per 100k")

## GDP and suicide
country_mean_gdp <- data %>%
  group_by(country, continent) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000, 
            gdp_per_capita = mean(gdp_per_capita))

ggplot(country_mean_gdp, aes(x = gdp_per_capita, y = suicide_per_100k, col = continent)) + 
  geom_point() + 
  scale_x_continuous(labels=scales::dollar_format(prefix="$"), breaks = seq(0, 70000, 10000)) + 
  labs(title = "Correlation between GDP (per capita) and Suicides per 100k", 
       subtitle = "Plot containing every country",
       x = "GDP (per capita)", 
       y = "Suicides per 100k", 
       col = "Continent") 

## Countries with high correlations of GDP and suicide

options(repr.plot.width = 10,rer.plot.height = 30)
country_cor_plot <- data %>%
  group_by(country, year) %>%
  summarise(gdp_per_capita = mean(gdp_per_capita), suicide_per_100k = mean(suicide_per_100k)) %>%
  group_by(country) %>%
  summarise(correlation = cor(gdp_per_capita, suicide_per_100k,method = 'spearman')) %>% 
  arrange(desc(correlation)) %>% 
  filter(correlation>0.5 | correlation < -0.5) %>% 
  ggplot(aes(x = country,y = correlation,col=correlation))+
  geom_segment(aes(x = country, y=0,xend = country, yend = correlation))+
  geom_point(stat='identity')+
  scale_color_gradient(low='#d8345f',high='#4cbbb9')+
  ylim(c(-1,1)) + coord_flip()
leaned_
country_cor_plot

################################
# Specify explanatory and outcome variables and model formula

vars = c("continent", "population", "country", "sex", "year", "age", "gdp_per_capita")
outcome = "suicide_rate_log"
(fmla = as.formula(paste(outcome, "~", paste(vars, collapse = " + "))))

# Variable transformation
data <- data %>%
  mutate(suicide_rate_log=log(1+suicide_per_100k))

# Split to training and testing datasets
#set.seed(1, sample.kind="Rounding")

test_index = createDataPartition(y = data$suicide_rate_log, times = 1, 
                                 p = 0.2, list = FALSE)
train = data[-test_index,]
test = data[test_index,]


# Linear regression

lm1 <- train %>% 
  lm(fmla, data=.)
y_pred_lm = predict(lm1, newdata = test)
#Performance metrics
mae_lm = mae(test[[12]], y_pred_lm)
rmse_lm = rmse(test[[12]], y_pred_lm)
mae_lm
rmse_lm

#test$lm <- predict(lm1, newdata = test)

# Random forests

set.seed(1)
rf = randomForest(fmla, data = train,
                  ntree = 500)
#Predicting the test values
y_pred_rf = predict(rf, newdata = test)
#Performance metrics
mae_rf = mae(test[[8]], y_pred_rf)
rmse_rf = rmse(test[[8]], y_pred_rf)
mae_rf
rmse_rf


#set.seed(1, sample.kind="Rounding")

#rf1 = ranger(fmla, # formula 
            #train, # data
           #  num.trees = 500, 
           #  respect.unordered.factors = "order",
            # seed = 1)

#test$lm <- predict(lm1, newdata = test)
#test$rf1 <- predict(rf1, newdata = test)
#rmse_rf1 = rmse(test[[12]], test$rf1 )
#rmse_rf1

case1 <- test %>% gather(key=model, value=log_pred, y_pred_lm, y_pred_rf) %>%
  mutate(pred=exp(log_pred),
         residuals=suicide_per_100k-pred) %>%
  group_by(model) %>%
  summarize(rmse=sqrt(mean(residuals^2)))

case1

#Saving the model
saveRDS(rf, file = "./rf.rda")

test %>% mutate(lm=exp(lm), rf=exp(rf)) %>%
  gather(key=valuetype, value=rate, suicide_per_100k, lm, rf) %>%
  mutate(suicides=rate*population/100000) %>%
  group_by(year, valuetype) %>%
  mutate(rate_year=sum(suicides)*100000/sum(population)) %>%
  ggplot(aes(year, rate_year, col=valuetype)) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1985, 2016, 2)) +
  theme(legend.position = 'bottom', axis.text.x = element_text(angle = 45))

test


