# GLOBAL SUICIDE RATE ANALYSIS

library(tidyverse) # General plotting 
library(ggalt) # For dumbbell plots
library(countrycode) # Continent
library(rworldmap) # Country-level heat maps
library(gridExtra) # plots
library(broom) # significant trends within countries

# 1) IMPORTING DATA - Data taken from WHO(and Kaggle)
data <- read_csv("C:\\Users\\91978\\Desktop\\SUICIDE RATE ANALYSIS\\master.csv") 

data <- data %>% 
  select(-c(`HDI for year`, `suicides/100k pop`)) %>%
  rename(gdp_for_year = `gdp_for_year ($)`, 
         gdp_per_capita = `gdp_per_capita ($)`, 
         country_year = `country-year`) %>%
  as.data.frame()

# 2) Unusual data is removed
# a) 2016 data is excluded - incomplete
data <- data %>%
  filter(year != 2016) %>%
  select(-country_year)


# b) excluding countries with <= 3 years of data:
minimum_years <- data %>%
  group_by(country) %>%
  summarize(rows = n(), 
            years = rows / 12) %>%
  arrange(years)

data <- data %>%
  filter(!(country %in% head(minimum_years$country, 7)))


# 3)
data$age <- gsub(" years", "", data$age)
data$sex <- ifelse(data$sex == "male", "Male", "Female")


# getting continent data
data$continent <- countrycode(sourcevar = data[, "country"],
                              origin = "country.name",
                              destination = "continent")

# Nominal factors
data_nominal <- c('country', 'sex', 'continent')
data[data_nominal] <- lapply(data[data_nominal], function(x){factor(x)})


# Making age ordinal
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


# the global rate over the time period
global_average <- (sum(as.numeric(data$suicides_no)) / sum(as.numeric(data$population))) * 100000

glimpse(data)   # Final data


# Plot 1 - Global Suicides from 1985 - 2015
data %>%
  group_by(year) %>%
  summarize(population = sum(population), 
            suicides = sum(suicides_no), 
            suicides_per_100k = (suicides / population) * 100000) %>%
  ggplot(aes(x = year, y = suicides_per_100k)) + 
  geom_line(col = "deepskyblue3", size = 1) + 
  geom_point(col = "deepskyblue3", size = 2) + 
  geom_hline(yintercept = global_average, linetype = 2, color = "grey35", size = 1) +
  labs(title = "GLOBAL SUICIDES (per 100k)",
       subtitle = "Trend over time, 1985 - 2015.",
       x = "Year", 
       y = "Suicides per 100k") + 
  scale_x_continuous(breaks = seq(1985, 2015, 2)) + 
  scale_y_continuous(breaks = seq(10, 20))


# Plot 2 - By Continent
continent <- data %>%
  group_by(continent) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  arrange(suicide_per_100k)

continent$continent <- factor(continent$continent, ordered = T, levels = continent$continent)

continent_plot <- ggplot(continent, aes(x = continent, y = suicide_per_100k, fill = continent)) + 
  geom_bar(stat = "identity") + 
  labs(title = "GLOBAL SUICIDES (per 100k), by Continent",
       x = "Continent", 
       y = "Suicides per 100k", 
       fill = "Continent") +
  theme(legend.position = "none", title = element_text(size = 10)) + 
  scale_y_continuous(breaks = seq(0, 20, 1), minor_breaks = F)


continent_time <- data %>%
  group_by(year, continent) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)

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


# Plot 3 - By Sex
sex_plot <- data %>%
  group_by(sex) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  ggplot(aes(x = sex, y = suicide_per_100k, fill = sex)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Global suicides (per 100k), by Sex",
       x = "Sex", 
       y = "Suicides per 100k") +
  theme(legend.position = "none") + 
  scale_y_continuous(breaks = seq(0, 25), minor_breaks = F)

# with year
sex_time_plot <- data %>%
  group_by(year, sex) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  ggplot(aes(x = year, y = suicide_per_100k, col = factor(sex))) + 
  facet_grid(sex ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "Trends Over Time, by Sex", 
       x = "Year", 
       y = "Suicides per 100k", 
       color = "Sex") + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F)

grid.arrange(sex_plot, sex_time_plot, ncol = 2)

# Plot 4 - By Age
age_plot <- data %>%
  group_by(age) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  ggplot(aes(x = age, y = suicide_per_100k, fill = age)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Global suicides per 100k, by Age",
       x = "Age", 
       y = "Suicides per 100k") +
  theme(legend.position = "none") + 
  scale_y_continuous(breaks = seq(0, 30, 1), minor_breaks = F)

#  with year
age_time_plot <- data %>%
  group_by(year, age) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  ggplot(aes(x = year, y = suicide_per_100k, col = age)) + 
  facet_grid(age ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "Trends Over Time, by Age", 
       x = "Year", 
       y = "Suicides per 100k", 
       color = "Age") + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F)

grid.arrange(age_plot, age_time_plot, ncol = 2)



country <- data %>%
  group_by(country, continent) %>%
  summarize(n = n(), 
            suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  arrange(desc(suicide_per_100k))

country$country <- factor(country$country, 
                          ordered = T, 
                          levels = rev(country$country))
# Plot 5
ggplot(country, aes(x = country, y = suicide_per_100k, fill = continent)) + 
  geom_bar(stat = "identity") + 
  geom_hline(yintercept = global_average, linetype = 2, color = "grey35", size = 1) +
  labs(title = "Global suicides per 100k, by Country",
       x = "Country", 
       y = "Suicides per 100k", 
       fill = "Continent") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 45, 2)) + 
  theme(legend.position = "bottom")

# Plot 6
country <- data %>%
  group_by(country) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)

countrydata <- joinCountryData2Map(country, joinCode = "NAME", nameJoinColumn = "country")

par(mar=c(0, 0, 0, 0)) # margins

mapCountryData(countrydata, 
               nameColumnToPlot="suicide_per_100k", 
               mapTitle="", 
               colourPalette = "heat", 
               oceanCol="lightblue", 
               missingCountryCol="grey65", 
               catMethod = "pretty")


# Linear Trends
country_year <- data %>%
  group_by(country, year) %>%
  summarize(suicides = sum(suicides_no), 
            population = sum(population), 
            suicide_per_100k = (suicides / population) * 100000, 
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

# plot 7
ggplot(country_year_sig_trends, aes(x=country, y=estimate, col = estimate)) + 
  geom_point(stat='identity', size = 4) +
  geom_hline(yintercept = 0, col = "grey", size = 1) +
  scale_color_gradient(low = "green", high = "red") +
  geom_segment(aes(y = 0, 
                   x = country, 
                   yend = estimate, 
                   xend = country), size = 1) +
  labs(title="Change per year (Suicides per 100k)", 
       x = "Country", y = "Change Per Year (Suicides per 100k)") +
  scale_y_continuous(breaks = seq(-2, 2, 0.2), limits = c(-1.5, 1.5)) +
  theme(legend.position = "none") +
  coord_flip()

# Plot 8
#Steepest *increasing* trends:
top12_increasing <- tail(country_year_sig_trends$country, 12)

country_year %>%
  filter(country %in% top12_increasing) %>%
  ggplot(aes(x = year, y = suicide_per_100k, col = country)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~ country) + 
  theme(legend.position = "none") + 
  labs(title="12 Steepest Increasing Trends", 
       x = "Year", 
       y = "Suicides per 100k")

# Plot 9
#Steepest *decreasing* trends:
top12_decreasing <- head(country_year_sig_trends$country, 12)

country_year %>%
  filter(country %in% top12_decreasing) %>%
  ggplot(aes(x = year, y = suicide_per_100k, col = country)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~ country) + 
  theme(legend.position = "none") + 
  labs(title="12 Steepest Decreasing Trends", 
       x = "Year", 
       y = "Suicides per 100k")


# Plot 10
# Gender differences, by Continent
data %>%
  group_by(continent, sex) %>%
  summarize(n = n(), 
            suicides = sum(as.numeric(suicides_no)), 
            population = sum(as.numeric(population)), 
            suicide_per_100k = (suicides / population) * 100000) %>%
  ggplot(aes(x = continent, y = suicide_per_100k, fill = sex)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_hline(yintercept = global_average, linetype = 2, color = "grey35", size = 1) +
  labs(title = "Gender Disparity, by Continent",
       x = "Continent", 
       y = "Suicides per 100k", 
       fill = "Sex") +
  coord_flip()


# Gender differences, by Country
country_long <- data %>%
  group_by(country, continent) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  mutate(sex = "OVERALL")

# by country, continent, sex

sex_country_long <- data %>%
  group_by(country, continent, sex) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)


sex_country_wide <- sex_country_long %>%
  spread(sex, suicide_per_100k) %>%
  arrange(Male - Female)


sex_country_wide$country <- factor(sex_country_wide$country, 
                                   ordered = T, 
                                   levels = sex_country_wide$country)

sex_country_long$country <- factor(sex_country_long$country, 
                                   ordered = T, 
                                   levels = sex_country_wide$country) # using the same order


# Plot 11
ggplot(sex_country_wide, aes(y = country, color = sex)) + 
  geom_dumbbell(aes(x=Female, xend=Male), color = "grey", size = 1) + 
  geom_point(data = sex_country_long, aes(x = suicide_per_100k), size = 3) +
  geom_point(data = country_long, aes(x = suicide_per_100k)) + 
  geom_vline(xintercept = global_average, linetype = 2, color = "grey35", size = 1) +
  theme(axis.text.y = element_text(size = 8), 
        legend.position = c(0.85, 0.2)) + 
  scale_x_continuous(breaks = seq(0, 80, 10)) +
  labs(title = "Gender Disparity, by Continent & Country", 
       subtitle = "Ordered by difference in deaths per 100k.", 
       x = "Suicides per 100k", 
       y = "Country", 
       color = "Sex")

# Plot 12
country_gender_prop <- sex_country_wide %>%
  mutate(Male_Proportion = Male / (Female + Male)) %>%
  arrange(Male_Proportion)

sex_country_long$country <- factor(sex_country_long$country, 
                                   ordered = T,
                                   levels = country_gender_prop$country)

ggplot(sex_country_long, aes(y = suicide_per_100k, x = country, fill = sex)) + 
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportions of suicides that are Male & Female, by Country", 
       x = "Country", 
       y = "Suicides per 100k",
       fill = "Sex") + 
  coord_flip()


# Plot 13
# Age differences, by Continent
data %>%
  group_by(continent, age) %>%
  summarize(n = n(), 
            suicides = sum(as.numeric(suicides_no)), 
            population = sum(as.numeric(population)), 
            suicide_per_100k = (suicides / population) * 100000) %>%
  ggplot(aes(x = continent, y = suicide_per_100k, fill = age)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_hline(yintercept = global_average, linetype = 2, color = "grey35", size = 1) +
  labs(title = "Age Disparity, by Continent",
       x = "Continent", 
       y = "Suicides per 100k", 
       fill = "Age")


# As a country gets richer, does it's suicide rate decrease?

country_year_gdp <- data %>%
  group_by(country, year) %>%
  summarize(gdp_per_capita = mean(gdp_per_capita))

country_year_gdp_corr <- country_year_gdp %>%
  ungroup() %>%
  group_by(country) %>%
  summarize(year_gdp_correlation = cor(year, gdp_per_capita))

# Plot 14
# Do richer countries have a higher rate of suicide?
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


model <- lm(suicide_per_100k ~ gdp_per_capita, data = country_mean_gdp)

