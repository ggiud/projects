#------------------------------
# T E S I
#------------------------------
library(tidyverse)

set.seed(100)
n <- 1000

p <- rpois(n, 18)
(tib1 <- tibble(pois = p))

x <- 1:n
dist_poi <- data.frame(x = x, y = dpois(x, lambda = 18))

p1 <- ggplot(tib1, aes(x = p))+
       geom_bar(aes(y = after_stat(prop)), color = 'white', fill = 'slateblue', alpha = 0.6)+
       geom_point(data = dist_poi, aes(x = x, y = y), color = 'navy', size = 1) +
       scale_x_continuous(limits = c(min(tib1$pois), max(tib1$pois) + 25))+
       labs(title = 'ISTOGRAMMA DISTRIBUZIONE POISSON') +
       theme_minimal()


bn <- rnbinom(n, 18, 0.5)
(tib2 <- tibble(bineg = bn))

x <- 1:n
dist_bineg <- data.frame(x = x, y = dnbinom(x, 18, 0.5))

p2 <- ggplot(tib2, aes(x = bn))+
       geom_bar(aes(y = after_stat(prop)), color = 'white', fill = 'indianred', alpha = 0.6)+
       geom_point(data = dist_bineg, aes(x = x, y = y), color = 'red', size = 1) +
       scale_x_continuous(limits = c(min(tib2$bineg), max(tib2$bineg) + 25))+
       labs(title = 'ISTOGRAMMA DISTRIBUZIONE BINOMIALE NEGATIVA') +
       theme_minimal()

library(gridExtra)
grid.arrange(p1, p2)


######################################
# grafico con mu che varia
######################################

mu <- c(0.5,1,2,5,10) 
y <- 0:10 
df8.1 <- data.frame()

for (i in 1:length(mu)) {
  p <- dpois(y, mu[i]) 
  temp_df <- data.frame("y" = y, "p" = p, "mu" = mu[i])
  df8.1 <- rbind(df8.1, temp_df)
}

a1 <- ggplot(df8.1, aes(x = y, y = p, colour = factor(mu))) +
  geom_line(linewidth = 1, show.legend = F) +
  geom_point(show.legend = F) +
  labs(x = '', y = '', color = "Mean values", title = 'Distribuzione NB2 con alpha = 0') +
  scale_color_manual(values = c('seagreen2', 'gold1', 'darkorchid','blue2', 'violet'))+
  theme_get()

  

mu <- c(0.5,1,2,5,10)
alpha <- .5 
amu <- mu*alpha 
 
df8.2 <- data.frame() 
 
for (i in 1:length(mu)) { 
ynb2 <- exp(y*log(amu[i]/(1+amu[i])) - (1/alpha)*log(1+amu[i]) 
        + log(gamma(y +1/alpha)) - log(gamma(y+1)) - log(gamma(1/alpha))) 
temp_df <- data.frame("y" = y, "ynb2" = ynb2, "mu" = mu[i]) 
df8.2 <- rbind(df8.2, temp_df) 
} 
 
a2 <- ggplot(df8.2, aes(x = y, y = ynb2, colour = factor(mu))) + 
  geom_line(linewidth = 1, show.legend = F) +
  geom_point(show.legend = F) +
  labs(x = '', y = '', color = "Mean values", title = 'Distribuzione NB2 con alpha = 0.5') +
  scale_color_manual(values = c('seagreen2', 'gold1', 'darkorchid','blue2', 'violet'))+
  theme_get()
  
  
mu <- c(0.5,1,2,5,10)
alpha2 <- 2 
amu2 <- mu*alpha2 

df8.3 <- data.frame() 

for (i in 1:length(mu)) { 
  ynb2.2 <- exp(y*log(amu2[i]/(1+amu2[i])) - (1/alpha2)*log(1+amu2[i]) 
              + log(gamma(y +1/alpha2)) - log(gamma(y+1)) - log(gamma(1/alpha2))) 
  temp_df <- data.frame("y" = y, "ynb2" = ynb2.2, "mu" = mu[i]) 
  df8.3 <- rbind(df8.3, temp_df) 
} 

a3 <- ggplot(df8.3, aes(x = y, y = ynb2, colour = factor(mu))) + 
  geom_line(linewidth = 1, show.legend = F) +
  geom_point(show.legend = F) +
  labs(x = '', y = '', color = "Mean values", title = 'Distribuzione NB2 con alpha = 2') +
  scale_color_manual(values = c('seagreen2', 'gold1', 'darkorchid','blue2', 'violet'))+
  theme_get()

  
mu <- c(0.5,1,2,5,10)
alpha3 <- 3 
amu3 <- mu*alpha3 

df8.4 <- data.frame() 

for (i in 1:length(mu)) { 
  ynb2.3 <- exp(y*log(amu3[i]/(1+amu3[i])) - (1/alpha3)*log(1+amu3[i]) 
              + log(gamma(y +1/alpha3)) - log(gamma(y+1)) - log(gamma(1/alpha3))) 
  temp_df <- data.frame("y" = y, "ynb2" = ynb2.3, "mu" = mu[i]) 
  df8.4 <- rbind(df8.4, temp_df) 
} 

a4 <- ggplot(df8.4, aes(x = y, y = ynb2, colour = factor(mu))) + 
  geom_line(linewidth = 1) +
  geom_point() +
  labs(x = '', y = '', color = "mu", title = 'Distribuzione NB2 con alpha = 3') +
  scale_color_manual(values = c('seagreen2', 'gold1', 'darkorchid','blue2', 'violet')) +
  theme(legend.direction = 'horizontal')

library(cowplot)
legend_a4 <- suppressWarnings(get_legend(a4))

grid.arrange(a1, a2, a3, a4 + theme(legend.position = 'none'), bottom = legend_a4)



###############################
# grafico con alpha che varia
##############################

obs <- 15
alpha <- c(.01, .67, 1, 1.5, 3)
y <- 0:10
mu <- 0.5
amu_1 <- mu * alpha

data <- data.frame(
  y = rep(y, times = length(alpha)),
  alpha = rep(alpha, each = length(y))
)


data <- data %>%
  mutate(
    amu_1 = mu * alpha,
    ynb2 = exp(y * log(amu_1 / (1 + amu_1)) - (1 / alpha) * log(1 + amu_1) +
        lgamma(y + 1 / alpha) - lgamma(y + 1) - lgamma(1 / alpha)))


m1 <- ggplot(data, aes(x = y, y = ynb2, color = as.factor(alpha))) +
        geom_line(linewidth = 1, show.legend = F) +
        geom_point(show.legend = F)+
        labs(title = "Distribuzione NB2 con mu = 0.5", x = "", y = "", color = "alpha" ) +
        scale_color_manual(values = c('seagreen2', 'gold1', 'darkorchid','blue2', 'violet')) +
        theme(legend.direction = 'horizontal')




mu_2 <- 2
amu_2 <- mu_2 * alpha

data_2 <- data.frame(
  y = rep(y, times = length(alpha)),
  alpha = rep(alpha, each = length(y)))

data_2 <- data_2 %>%
  mutate(amu_2 = mu_2 * alpha,
         ynb2 = exp(y * log(amu_2 / (1 + amu_2)) - (1 / alpha) * log(1 + amu_2) +
                 lgamma(y + 1 / alpha) - lgamma(y + 1) - lgamma(1 / alpha)))

m2 <- ggplot(data_2, aes(x = y, y = ynb2, color = as.factor(alpha))) +
        geom_line(linewidth = 1, show.legend = F) +
        geom_point(show.legend = F)+
        labs(title = "Distribuzione NB2 con mu = 2", x = "", y = "", color = "alpha" ) +
        scale_color_manual(values = c('seagreen2', 'gold1', 'darkorchid','blue2', 'violet')) +
        ylim(y_limits)+
        theme(legend.direction = 'horizontal')



mu_3 <- 10
amu_3 <- mu_3 * alpha

data_3 <- data.frame(
  y = rep(y, times = length(alpha)),
  alpha = rep(alpha, each = length(y)))

data_3 <- data_3 %>%
  mutate(amu_3 = mu_3 * alpha,
         ynb2 = exp(y * log(amu_3 / (1 + amu_3)) - (1 / alpha) * log(1 + amu_3) +
                      lgamma(y + 1 / alpha) - lgamma(y + 1) - lgamma(1 / alpha)))

m3 <- ggplot(data_3, aes(x = y, y = ynb2, color = as.factor(alpha))) +
       geom_line(linewidth = 1, show.legend = F) +
       geom_point(show.legend = F)+
       labs(title = "Distribuzione NB2 con mu = 10", x = "", y = "", color = "alpha" ) +
       scale_color_manual(values = c('seagreen2', 'gold1', 'darkorchid','blue2', 'violet')) +
       ylim(y_limits)+
       theme(legend.direction = 'horizontal')


mu_4 <- 1
amu_4 <- mu_4 * alpha

data_4 <- data.frame(
  y = rep(y, times = length(alpha)),
  alpha = rep(alpha, each = length(y)))

data_4 <- data_4 %>%
  mutate(amu_4 = mu_4 * alpha,
         ynb2 = exp(y * log(amu_4 / (1 + amu_4)) - (1 / alpha) * log(1 + amu_4) +
                      lgamma(y + 1 / alpha) - lgamma(y + 1) - lgamma(1 / alpha)))

y_limits <- ggplot_build(m1)$layout$panel_scales_y[[1]]$range$range

m4 <- ggplot(data_4, aes(x = y, y = ynb2, color = as.factor(alpha))) +
         geom_line(linewidth = 1) +
         geom_point()+
         labs(title = "Distribuzione NB2 con mu = 1", x = "", y = "", color = "alpha" ) +
         scale_color_manual(values = c('seagreen2', 'gold1', 'darkorchid','blue2', 'violet')) +
         ylim(y_limits)+
         theme(legend.direction = 'horizontal')

library(cowplot)
legend_m4 <- suppressWarnings(get_legend(m4))

library(gridExtra)
grid.arrange(m1, m4 + theme(legend.position = 'none'), m2, m3, bottom = legend_m4)



#############################################################
# A P P L I C A Z I O N E, tamarrata
#############################################################

library(tidyverse)
setwd("C:\\Users\\giudi\\OneDrive\\Desktop\\uni\\tesi")
dati <- read.csv("covid data.csv")

colnames(dati)

anyNA(dati)
sum(is.na(dati))
sum(complete.cases(dati))

library(mice)
md.pattern(dati)

library(visdat)
vis_dat(dati)

levels(as.factor(dati$source)) # questo ha un solo livello elimino la variabile
                               # perché non ha senso tenerla

dati <- select(dati, - c(note, source))    # note ha solo NA

sum(is.na(dati))
sum(complete.cases(dati)) / nrow(dati) # 89% di casi completi

vis_dat(dati)
md.pattern(dati)

library(naniar)
library(UpSetR)
upset(as_shadow_upset(dati))

dati[which(is.na(dati$country_code)),] # manca il dato di country code perché stiamo
                                       # parlando dell'europa

levels(as_factor(dati$continent)) # i dati sono riferiti a tutti i paesi dell'europa 
                                   # togliamo questa colonna che è sempre pari a europe

# tolgo la variabile country code perché non mi restitutisce ulteriori informazioni
dati <- select(dati, -c(country_code, continent))

levels(as.factor(dati$indicator))

dati2 <- dati %>% 
            group_by(year_week, country) %>%
            pivot_wider(names_from = indicator, values_from = c(weekly_count, rate_14_day, cumulative_count), 
                      names_sep = '_')
dati2 <- as.data.frame(dati2)

upset(as_shadow_upset(dati2))
md.pattern(dati2)

vis_dat(dati2) +
  scale_fill_manual(values= c('blue', 'seagreen2', 'violet', 'darkorchid'))

vis_miss(dati2)

dati2 <- dati2[complete.cases(dati2),]

miss_var_summary(dati2) %>% knitr::kable()
head(miss_var_table(dati2)) 

prop_miss_case(dati)
pct_miss_case(dati)


miss_var_summary(dati) %>% knitr::kable()
miss_var_table(dati) 

prop_miss_case(dati)
pct_miss_case(dati)

(corr <- as_tibble(cor(dati2[,-c(1,3)])))


library(ggcorrplot)
ggcorrplot(cor(dati2[,-c(1,3)]), lab = TRUE,                 
           lab_size = 3, colors = c("gold1", "seagreen2", "blue2"),  
           ggtheme = theme_minimal()) 
  
library(GGally)
ggpairs(dati2[,-c(1,3)])  


library(heatmaply)  
dati_country <- dati2 %>%
                  filter(country != 'EU/EEA (total)') %>%
                  group_by(country) %>%
                  summarize(mean_population = mean(population),
                            sum_cases = sum(cumulative_count_cases),
                            sum_deaths = sum(cumulative_count_deaths))
  
heatmaply(normalize(dati_country[,-1]),
          labRow = dati_country$country,
          fontsize_row = 8, fontsize_col = 9,
          col = plasma(n = 200, begin = 0.2, end = 1),
          grid_gap = 0.6,
          main = 'HEATMAP CON VALORI NORMALIZZATI', ylab = 'nazioni',
          column_text_angle = 0,
          margins = c(30, 30, 70, 70),
          show_dendrogram = F,
          draw_cellnote = T, cellnote_textposition = 'middle center', cellnote_size = 7)


d1 <- ggplot(dati2, aes(x = weekly_count_deaths)) +
  geom_histogram(aes(y = ..density..), binwidth = 10, fill = "blue2", alpha = 0.7, color = 'white') +
  geom_density(aes(y = ..density..), color = "seagreen2", fill =  "seagreen2", alpha = 0.2, size = 1) +
  labs(title = "Distribuzione weekly count deaths dati completi",
       x = "Weekly Count Deaths",
       y = "Density") +
  scale_x_continuous(limits = c(0, 600)) + 
  theme_bw()

d2 <- ggplot(dati2, aes(x = weekly_count_cases)) +
  geom_histogram(aes(y = ..density..), binwidth = 100, fill = "blue2", alpha = 0.7, color = 'white') +
  geom_density(aes(y = ..density..),  color = "seagreen2", fill =  "seagreen2", alpha = 0.2, size = 1) +
  labs(title = "Dist. weekly count cases dati completi",
       x = "Weekly Count Cases",
       y = "Density") +
  scale_x_continuous(limits = c(0, 10000)) + 
  theme_get()


# dal momento che i dati con l'europa sembrano avere graficamente dei problemi togliamo 
# le unità statistiche con country = eu in modo tale da non considerare il totale di
# tutti i paesi e rieseguiamo tutte le analisi

dati2$country <- factor(dati2$country)
levels(dati2$country)

summary(dati2)

dati_no_eu <- filter(dati2, country != "EU/EEA (total)")
levels(factor(dati_no_eu$country))


tab <- xtabs(~ weekly_count_deaths, dati_no_eu)
ks.test(tab, 'ppois', mean(dati_no_eu$weekly_count_deaths))

d3 <- ggplot(dati_no_eu, aes(x = weekly_count_deaths)) +
  geom_histogram(aes(y = ..density..), binwidth = 10, fill = "blue2", alpha = 0.7, color = 'white') +
  geom_density(aes(y = ..density..),  color = "seagreen2", fill =  "seagreen2", alpha = 0.2, size = 1) +
  labs(title = "Distribuzione weekly count deaths dati no eu",
       x = "Weekly Count Deaths",
       y = "Density") +
  scale_x_continuous(limits = c(0, 600)) + 
  theme_bw()

d4 <- ggplot(dati_no_eu, aes(x = weekly_count_cases)) +
  geom_histogram(aes(y = ..density..), binwidth = 100, fill = "blue2", alpha = 0.7, color = 'white') +
  geom_density(aes(y = ..density..),  color = "seagreen2", fill =  "seagreen2", alpha = 0.2, size = 1) +
  labs(title = "Dist. weekly count cases dati no eu",
       x = "Weekly Count Cases",
       y = "Density") +
  scale_x_continuous(limits = c(0, 10000)) + 
  theme_get()

library(gridExtra)
grid.arrange(d1, d2, d3, d4)

grid.arrange(d1, d3)

b1 <- ggplot(dati_no_eu,  aes(y = weekly_count_cases)) + 
  geom_boxplot(color = 'blue2', outlier.shape = '*', outlier.size = 4, show.legend = T) +
  theme_bw() 

b2 <- ggplot(dati2,  aes(y = weekly_count_deaths)) + 
  geom_boxplot(color = 'violet', outlier.shape = '*', outlier.size = 4, show.legend = F) +
  theme_bw() 

b3 <- ggplot(dati2,  aes(y = population)) + 
  geom_boxplot(color = 'seagreen2', outlier.shape = '*', outlier.size = 4, show.legend = F) +
  theme_get() 

b4 <- ggplot(dati_no_eu,  aes(y = rate_14_day_cases)) + 
  geom_boxplot(color = 'darkorchid', outlier.shape = '*', outlier.size = 4, show.legend = F) +
  theme_bw()

b5 <- ggplot(dati_no_eu,  aes(y = rate_14_day_deaths)) + 
  geom_boxplot(color = 'gold1', outlier.shape = '*', outlier.size = 4, show.legend = F) +
  theme_bw() 

b6 <- ggplot(dati_no_eu,  aes(y = cumulative_count_deaths)) + 
  geom_boxplot(color = 'seagreen2', outlier.shape = '*', outlier.size = 4, show.legend = F) +
  theme_bw() 

b7 <- ggplot(dati_no_eu,  aes(y = cumulative_count_cases)) + 
  geom_boxplot(color = 'slateblue', outlier.shape = '*', outlier.size = 4, show.legend = F) +
  theme_bw() 


grid.arrange(b1, b2, b4, b5, b6, b7, nrow = 2)

min_max_normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
boxplot(normalize(dati2[-c(1, 3, 10, 11)]))
boxplot(as.data.frame(lapply(dati2[, -c(1, 3, 10, 11)], min_max_normalize)))

dati2_normalized <- as.data.frame(lapply(dati2[, -c(1, 3, 10, 11)], min_max_normalize))

# Convertiamo i dati normalizzati in formato long per ggplot2
dati2_long <- reshape2::melt(dati2_normalized)

# Creiamo il boxplot con colori diversi per ogni box
ggplot(dati2_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(show.legend = F, color = 'black', linewidth = 0.2, outlier.alpha = 0.5) +
  scale_fill_manual(values = c("seagreen2", "blue", "cyan", "violet", "blue2", "gold1", "darkorchid")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  labs(title = "Boxplot delle variabili normalizzate", x = "", y = "Valore Normalizzato")


################################################
# M O D E L L I
################################################

# divisione training test in base al tempo
dati2$year <- as.numeric(sub("-.*", "", dati2$year_week))
dati2$week <- as.numeric(sub(".*-", "", dati2$year_week))

# Ordina i dati in base alla colonna 'date_order'
dati2 <- dati2 %>%
           arrange(year, week)

split_index <- floor(0.8 * nrow(dati2))
train_data <- dati2[1:split_index, ]
test_data <- dati2[(split_index + 1):nrow(dati2), ]

cat("Training data shape: ", dim(train_data), "\n")
cat("Test data shape: ", dim(test_data), "\n")


# POISSON
mod_poisson <- glm(weekly_count_deaths ~ population + cumulative_count_cases + 
             weekly_count_cases + cumulative_count_deaths + country + rate_14_day_cases +
             rate_14_day_deaths, train_data, family = 'poisson')
summary(mod_poisson)

par(mfrow = c(2,2))
plot(mod_poisson)
par(mfrow = c(1,1))

(phi.hat_poi <- sum(residuals(mod_poisson, type = 'pearson')^2)/mod_poisson$df.residual)

mu_hat <- predict(mod_poisson, type = 'response')

residuals(mod_poisson)

library(car)
outlier_test <- outlierTest(mod_poisson)

outlier_indices <- which(outlier_test$bonf.p < 0.05)
dati_no_outlier <- train_data[-outlier_indices, ]

mod_poisson_no_out <- glm(weekly_count_deaths ~ population + cumulative_count_cases + 
                     weekly_count_cases + cumulative_count_deaths + country + rate_14_day_cases +
                     rate_14_day_deaths, dati_no_outlier, family = 'poisson')
summary(mod_poisson_no_out)

par(mfrow = c(2,2))
plot(mod_poisson_no_out)
par(mfrow = c(1,1))

(phi.hat_poi_no_out <- sum(residuals(mod_poisson_no_out, type = 'pearson')^2)/mod_poisson_no_out$df.residual)



# NEGATIVE BINOMIAL
library(MASS)
mod_nb <- glm.nb(weekly_count_deaths ~ population + cumulative_count_cases + 
                   weekly_count_cases + cumulative_count_deaths + country + rate_14_day_cases +
                   rate_14_day_deaths, train_data)

summary(mod_nb)

(phi.hat_nb <- sum(residuals(mod_nb, type = 'pearson')^2)/mod_nb$df.residual)

par(mfrow = c(2,2))
plot(mod_nb)
par(mfrow = c(1,1))

outlierTest(mod_nb)


# QUASIPOISSON
mod_qp <- glm(weekly_count_deaths ~ population + cumulative_count_cases + 
                weekly_count_cases + cumulative_count_deaths + country + rate_14_day_cases +
                rate_14_day_deaths, train_data, family = 'quasipoisson')
summary(mod_qp)

par(mfrow = c(2,2))
plot(mod_qp)
par(mfrow = c(1,1))


# divisione training e test togliendo dal dataset l'Europa
data_no_eu <- dati2[dati2$country != "EU/EEA (total)", ]

split_index2 <- floor(0.8 * nrow(data_no_eu))
train_data2 <- data_no_eu[1:split_index2, ]
test_data2 <- data_no_eu[(split_index2 + 1):nrow(data_no_eu), ]

cat("Training data shape: ", dim(train_data2), "\n")
cat("Test data shape: ", dim(test_data2), "\n")


sweden_data <- subset(data_no_eu, country == "Sweden")
summary(sweden_data)

library(car)
vif_values <- vif(mod_poi2)
print(vif_values)

selectt <- select(data_no_eu, -c(country, year_week, year, week))
library(caret)
highly_correlated <- findCorrelation(cor(selectt), cutoff = 0.9)
print(highly_correlated)

levels(droplevels(train_data2$country))


# POISSON 2
mod_poi2 <- glm(weekly_count_deaths ~ population + cumulative_count_cases + 
                     weekly_count_cases + cumulative_count_deaths + country + rate_14_day_cases +
                     rate_14_day_deaths, data = train_data2, family = poisson)
summary(mod_poi2)

par(mfrow = c(2,2))
plot(mod_poi2)
par(mfrow = c(1,1))

(phi.hat_poi2 <- sum(residuals(mod_poi2, type = 'pearson')^2)/mod_poi2$df.residual)

residualPlot(mod_poi2)

outlierTest(mod_poi2)

library(AER)
dispersiontest(mod_poi2)

## plot diagnostici con ggplot a confronto ##
library(broom)

model_data <- augment(mod_poisson)

# 1. Residuals vs Fitted
p1 <- ggplot(model_data, aes(.fitted, .resid)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = F, color = "seagreen2") +
  labs(title = "Residuals vs Fitted",
       x = "Fitted values",
       y = "Residuals") +
  theme_bw()

# 2. Normal Q-Q
p2 <- ggplot(model_data, aes(sample = .std.resid)) +
  stat_qq(alpha = 0.5) +
  stat_qq_line(linewidth = 1, color = "gold1") +
  labs(title = "Normal Q-Q",
       x = "Theoretical Quantiles",
       y = "Standardized Residuals") +
  theme_bw()

# 3. Scale-Location (Spread-Location)
p3 <- ggplot(model_data, aes(.fitted, sqrt(abs(.std.resid)))) +
  geom_point(alpha = 0.5) +
  geom_smooth( se = F, color = "blue2") +
  labs(title = "Scale-Location",
       x = "Fitted values",
       y = expression(sqrt(abs("Standardized Residuals")))) +
  theme_bw()

# 4. Residuals vs Leverage
p4 <- ggplot(model_data, aes(.hat, .std.resid)) +
  geom_point(alpha = 0.5) +
  geom_smooth( se = F, color = "violet") +
  labs(title = "Res. vs Leverage",
       x = "Leverage",
       y = "Standardized Residuals") +
  theme_bw()

grid.arrange(p1, p2, p3, p4)

model2_data <- augment(mod_poisson_no_out)

# 1. Residuals vs Fitted
p1_2 <- ggplot(model2_data, aes(.fitted, .resid)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = F, color = "seagreen2") +
  labs(title = "Residuals vs Fitted",
       x = "Fitted values",
       y = "Residuals") +
  theme_bw()

# 2. Normal Q-Q
p2_2 <- ggplot(model2_data, aes(sample = .std.resid)) +
  stat_qq(alpha = 0.5) +
  stat_qq_line(linewidth = 1, color = "gold1") +
  labs(title = "Normal Q-Q",
       x = "Theoretical Quantiles",
       y = "Standardized Residuals") +
  theme_bw()

# 3. Scale-Location (Spread-Location)
p3_2 <- ggplot(model2_data, aes(.fitted, sqrt(abs(.std.resid)))) +
  geom_point(alpha = 0.5) +
  geom_smooth( se = F, color = "blue2") +
  labs(title = "Scale-Location",
       x = "Fitted values",
       y = expression(sqrt(abs("Standardized Residuals")))) +
  theme_bw()

# 4. Residuals vs Leverage
p4_2 <- ggplot(model2_data, aes(.hat, .std.resid)) +
  geom_point(alpha = 0.5) +
  geom_smooth( se = F, color = "violet") +
  labs(title = "Res. vs Leverage",
       x = "Leverage",
       y = "Standardized Residuals") +
  theme_bw()


grid.arrange(p1_2, p2_2, p3_2, p4_2, nrow = 2)
grid.arrange(p1, p2, p3, p4, p1_2, p2_2, p3_2, p4_2, nrow = 2)


# NEGATIVE BINOMIAL 2
mod_nb2 <- glm.nb(weekly_count_deaths ~ population + cumulative_count_cases + 
                   weekly_count_cases + cumulative_count_deaths + country + rate_14_day_cases +
                   rate_14_day_deaths, train_data2)

summary(mod_nb2)

(phi.hat_nb2 <- sum(residuals(mod_nb2, type = 'pearson')^2)/mod_nb2$df.residual)

par(mfrow = c(2,2))
plot(mod_nb2)
par(mfrow = c(1,1))

outlierTest(mod_nb2)


# QUASIPOISSON 2
mod_qp2 <- glm(weekly_count_deaths ~ population + cumulative_count_cases + 
                weekly_count_cases + cumulative_count_deaths + country + rate_14_day_cases +
                rate_14_day_deaths, train_data2, family = 'quasipoisson')
summary(mod_qp2)

(phi.hat_qp2 <- sum(residuals(mod_qp2, type = 'pearson')^2)/mod_qp2$df.residual)


par(mfrow = c(2,2))
plot(mod_qp2)
par(mfrow = c(1,1))



# ZINB
library(pscl)
mod_zinb <- zeroinfl(weekly_count_deaths ~ population + cumulative_count_cases + 
                       weekly_count_cases + cumulative_count_deaths + country + rate_14_day_cases +
                       rate_14_day_deaths, data = train_data, dist = "negbin")
summary(mod_zinb)


# ZIP
mod_zip <- zeroinfl(weekly_count_deaths ~ population + cumulative_count_cases + 
                       weekly_count_cases + cumulative_count_deaths + country + rate_14_day_cases +
                       rate_14_day_deaths, data = train_data, dist = "pois")
summary(mod_zip)


# HURDLE
mod_hurdle <- hurdle(weekly_count_deaths ~ population + cumulative_count_cases + 
                      weekly_count_cases + cumulative_count_deaths + country + rate_14_day_cases +
                      rate_14_day_deaths, data = train_data, dist = "negbin")
summary(mod_hurdle)


# Standardizzo le variabili così da vedere se i problemi di multicollinearità vengono risolti
train_data3 <- train_data
train_data3$weekly_count_cases <- scale(train_data3$weekly_count_cases)
train_data3$cumulative_count_deaths <- scale(train_data3$cumulative_count_deaths)
train_data3$rate_14_day_cases <- scale(train_data3$rate_14_day_cases)
train_data3$rate_14_day_deaths <- scale(train_data3$rate_14_day_deaths)
train_data3$population <- scale(train_data3$population)


# ZINB 2
mod_zinb2 <- zeroinfl(weekly_count_deaths ~ population + weekly_count_cases + cumulative_count_deaths + 
                       rate_14_day_cases + rate_14_day_deaths, data = train_data3, dist = "negbin")
summary(mod_zinb2)

# ZIP 2
mod_zip2 <- zeroinfl(weekly_count_deaths ~ population + weekly_count_cases + cumulative_count_deaths + 
                        rate_14_day_cases + rate_14_day_deaths, train_data3, dist = "pois")
summary(mod_zip2)

# HURDLE 2
mod_hurdle2 <- hurdle(weekly_count_deaths ~ population + cumulative_count_cases + 
                       weekly_count_cases + cumulative_count_deaths + rate_14_day_cases +
                       rate_14_day_deaths, data = train_data3, dist = "negbin")
summary(mod_hurdle2)


# AIC e BIC a confronto
tab_confronto <- cbind(c(AIC(mod_poisson), AIC(mod_poisson_no_out), AIC(mod_poi2), AIC(mod_nb), AIC(mod_nb2), 
                         AIC(mod_zip2), AIC(mod_zinb2), AIC(mod_qp2), AIC(mod_hurdle2)),
                       c(BIC(mod_poisson), BIC(mod_poisson_no_out), BIC(mod_poi2), BIC(mod_nb), BIC(mod_nb2),
                         BIC(mod_zip2), BIC(mod_zinb2), BIC(mod_qp2), BIC(mod_hurdle2)))



colnames(tab_confronto) <- c('AIC', 'BIC')
rownames(tab_confronto) <- c('Poisson', 'Poisson no outliers', 'Poisson no UE',
                             'Negative Binomial', 'Negative Binomial no UE',
                             'Zero Inflated Poisson', 'Zero Inflated NegBin',
                             'Quasi-Poisson', 'Hurdle model')
tab_confronto



# GRAFICO DI CONFRONTO

predicted <- predict(mod_nb, newdata = test_data, type = "response")
uno <- ggplot(test_data, aes(x = weekly_count_deaths, y = predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "gold1", linewidth = 1, linetype = 'dashed') +
  labs(title = "Negative Binomial",
       x = "Valori reali (test data)",
       y = "Valori previsti") +
  scale_y_continuous(limits = c(0,1500))+
  scale_x_continuous(limits = c(0,1500))+
  theme_bw()

predicted_poi <- predict(mod_poisson, newdata = test_data, type = "response")
due <- ggplot(test_data, aes(x = weekly_count_deaths, y = predicted_poi)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "blue2", linewidth = 1, linetype = "dashed") +
  labs(title = "Poisson",
       x = "Valori reali (test data)",
       y = "Valori previsti") +
  scale_y_continuous(limits = c(0,1500))+
  scale_x_continuous(limits = c(0,1500))+
  theme_bw()



predicted_qp <- predict(mod_qp, newdata = test_data, type = "response")
tre <- ggplot(test_data, aes(x = weekly_count_deaths, y = predicted_qp)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "seagreen2", linewidth = 1, linetype = "dashed") +
  labs(title = "Quasi-Poisson",
       x = "Valori reali (test data)",
       y = "Valori previsti") +
  scale_y_continuous(limits = c(0,1500))+
  scale_x_continuous(limits = c(0,1500))+
  theme_bw()


predicted_zinb <- predict(mod_zinb, newdata = test_data, type = "response")
quattro <- ggplot(test_data, aes(x = weekly_count_deaths, y = predicted_zinb)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "violet", linewidth = 1, linetype = "dashed") +
  labs(title = "ZINB",
       x = "Valori reali (test data)",
       y = "Valori previsti") +
  scale_y_continuous(limits = c(0,1500))+
  scale_x_continuous(limits = c(0,1500))+
  theme_bw()


predicted_zip <- predict(mod_zip, newdata = test_data, type = "response")
cinque <- ggplot(test_data, aes(x = weekly_count_deaths, y = predicted_zip)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "darkcyan", linewidth = 1, linetype = "dashed") +
  labs(title = "ZIP",
       x = "Valori reali (test data)",
       y = "Valori previsti") +
  scale_y_continuous(limits = c(0,1500))+
  scale_x_continuous(limits = c(0,1500))+
  theme_bw()


predicted_hurdle <- predict(mod_hurdle, newdata = test_data, type = "response")
sei <- ggplot(test_data, aes(x = weekly_count_deaths, y = predicted_hurdle)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "darkorchid", linewidth = 1, linetype = "dashed") +
  labs(title = "Hurdle",
       x = "Valori reali (test data)",
       y = "Valori previsti") +
  scale_y_continuous(limits = c(0,1500))+
  scale_x_continuous(limits = c(0,1500))+
  theme_bw()

grid.arrange(uno, due, tre, quattro, cinque, sei, ncol = 3)


# metriche di confronto
library(Metrics)

mse_poisson  <- mse(test_data$weekly_count_deaths, predicted_poi)
mse_nb       <- mse(test_data$weekly_count_deaths, predicted)
mse_qp       <- mse(test_data$weekly_count_deaths, predicted_qp)
mse_zip      <- mse(test_data$weekly_count_deaths, predicted_zip)
mse_zinb     <- mse(test_data$weekly_count_deaths, predicted_zinb)
mse_hurdle   <- mse(test_data$weekly_count_deaths, predicted_hurdle)

rmse_poisson <- round(rmse(test_data$weekly_count_deaths, predicted_poi),3)
rmse_nb      <- round(rmse(test_data$weekly_count_deaths, predicted),3)
rmse_qp      <- round(rmse(test_data$weekly_count_deaths, predicted_qp),3)
rmse_zip     <- round(rmse(test_data$weekly_count_deaths, predicted_zip),3)
rmse_zinb    <- round(rmse(test_data$weekly_count_deaths, predicted_zinb),3)
rmse_hurdle  <- round(rmse(test_data$weekly_count_deaths, predicted_hurdle),3)

mae_poisson  <- mae(test_data$weekly_count_deaths, predicted_poi)
mae_nb       <- mae(test_data$weekly_count_deaths, predicted)
mae_qp       <- mae(test_data$weekly_count_deaths, predicted_qp)
mae_zip      <- mae(test_data$weekly_count_deaths, predicted_zip)
mae_zinb     <- mae(test_data$weekly_count_deaths, predicted_zinb)
mae_hurdle   <- mae(test_data$weekly_count_deaths, predicted_hurdle)

r2_poisson <- round(cor(test_data$weekly_count_deaths, predicted_poi)^2,3)
r2_nb      <- round(cor(test_data$weekly_count_deaths, predicted)^2,3)
r2_qp      <- round(cor(test_data$weekly_count_deaths, predicted_qp)^2,3)
r2_zip     <- round(cor(test_data$weekly_count_deaths, predicted_zip)^2,3)
r2_zinb    <- round(cor(test_data$weekly_count_deaths, predicted_zinb)^2,3)
r2_hurdle  <- round(cor(test_data$weekly_count_deaths, predicted_hurdle),3)


tab_metriche <- cbind(c(mse_nb, mse_poisson, mse_qp, mse_zinb, mse_zip, mse_hurdle),
                      c(rmse_nb, rmse_poisson, rmse_qp, rmse_zinb, rmse_zip, rmse_hurdle),
                      c(round(mae_nb, 3), round(mae_poisson, 3), round(mae_qp, 3), round(mae_zinb, 3), round(mae_zip, 3), round(mse_hurdle, 3)),
                      c(r2_nb, r2_poisson, r2_qp, r2_zinb, r2_zip, r2_hurdle))                

colnames(tab_metriche) <- c('MSE', 'RMSE', 'MAE', expression(("R")^2))
rownames(tab_metriche) <- c('Negative Binomial', 'Poisson', 'Quasi-Poisson', 
                            'Zero Inflated Negative Binomial','Zero Inflated Poisson',
                            'Hurdle')

tab_metriche

