############################################################################
############################################################################
###                                                                      ###
###                             FAZEKAS MÁRK                             ###
###                  TÖBBVÁLTOZÓS ADATELEMZÉSI MODELLEK                  ###
###                            (MAME039LMSB)                             ###
###                                                                      ###
############################################################################
############################################################################

# commenting formats
# https://cran.r-project.org/web/packages/bannerCommenter/vignettes/Banded_comment_maker.pdf

## Set Working Directory
setwd("/Users/markfazekas/Documents/00_Code/R/HAZIDOLGOZAT/mariokart-r")

## Install Packages
# install.packages("janitor") # keeping an example line here

## Use Packages
library(readr)
library(janitor)
library(dplyr)
library(stringr)
library(ggplot2)
library(psych)
library(tidyr)
library(RcmdrMisc)
library(rcompanion)
library(questionr)


######################
##  Data Preparation
######################


##:::::::::::
##  Drivers
##:::::::::::

# olvassuk be az adatokat a helyes formátummal
drivers <- read_delim(
  "CSV/drivers.csv",
  delim = ";",
  escape_double = FALSE,
  col_types = cols(
    Weight = col_integer(),
    Acceleration = col_integer(),
    `On-Road traction` = col_integer(),
    `Off-Road Traction` = col_integer(),
    `Mini-Turbo` = col_integer(),
    `Ground Speed` = col_integer(),
    `Water Speed` = col_integer(),
    `Anti-Gravity Speed` = col_integer(),
    `Air Speed` = col_integer(),
    `Ground Handling` = col_integer(),
    `Water Handling` = col_integer(),
    `Anti-Gravity Handling` = col_integer(),
    `Air Handling` = col_integer()
  ),
  trim_ws = TRUE
)

# egységesítsük az oszlop neveket
drivers <- janitor::clean_names(drivers, "snake")

# vegyük ki a zárójeleket
drivers$driver <- gsub("[()]", "", drivers$driver)

# csoportosítsuk az azonos rekordokat, amik csak "névben" különböznek
# drivers <- drivers %>%
#   group_by_if(is.numeric) %>% #group by all numeric columns
#   summarise_at(vars(-one_of(names(drivers)[is.numeric(drivers)])), paste, collapse = ", ") %>% #concatenate the text variable
#   summarise_all(first) #keep the first value of numeric columns
# drivers <- drivers %>%
#   mutate(number_of_recs = str_count(driver, ",") + 1)

# extend the table with the missing size information
drivers <- drivers %>%
  mutate(
    size = case_when(
      str_detect(driver, 'Baby Daisy') ~ 'small',
      str_detect(driver, 'Baby Luigi') ~ 'small',
      str_detect(driver, 'Baby Mario') ~ 'small',
      str_detect(driver, 'Baby Peach') ~ 'small',
      str_detect(driver, 'Baby Rosalina') ~ 'small',
      str_detect(driver, 'Bowser') ~ 'large',
      str_detect(driver, 'Bowser Jr.') ~ 'small',
      str_detect(driver, 'Cat Peach') ~ 'medium',
      str_detect(driver, 'Daisy') ~ 'medium',
      str_detect(driver, 'Donkey Kong') ~ 'large',
      str_detect(driver, 'Dry Bones') ~ 'small',
      str_detect(driver, 'Dry Bowser') ~ 'large',
      str_detect(driver, 'Gold Mario') ~ 'medium',
      str_detect(driver, 'Iggy') ~ 'medium',
      str_detect(driver, 'Inkling Boy') ~ 'medium',
      str_detect(driver, 'Inkling Girl') ~ 'medium',
      str_detect(driver, 'Isabelle') ~ 'small',
      str_detect(driver, 'King Boo') ~ 'large',
      str_detect(driver, 'Koopa Troopa') ~ 'small',
      str_detect(driver, 'Lakitu') ~ 'small',
      str_detect(driver, 'Larry') ~ 'small',
      str_detect(driver, 'Lemmy') ~ 'small',
      str_detect(driver, 'Link') ~ 'large',
      str_detect(driver, 'Ludwig') ~ 'medium',
      str_detect(driver, 'Luigi') ~ 'medium',
      str_detect(driver, 'Mario') ~ 'medium',
      str_detect(driver, 'Metal Mario') ~ 'medium',
      str_detect(driver, 'Mii medium') ~ 'medium',
      str_detect(driver, 'Morton') ~ 'large',
      str_detect(driver, 'Peach') ~ 'medium',
      str_detect(driver, 'Pink Gold Peach') ~ 'medium',
      str_detect(driver, 'Rosalina') ~ 'large',
      str_detect(driver, 'Roy') ~ 'large',
      str_detect(driver, 'Shy Guy') ~ 'small',
      str_detect(driver, 'Tanooki Mario') ~ 'medium',
      str_detect(driver, 'Toad') ~ 'small',
      str_detect(driver, 'Toadette') ~ 'small',
      str_detect(driver, 'Villager female') ~ 'medium',
      str_detect(driver, 'Villager male') ~ 'medium',
      str_detect(driver, 'Waluigi') ~ 'large',
      str_detect(driver, 'Wario') ~ 'large',
      str_detect(driver, 'Wendy') ~ 'small',
      str_detect(driver, 'Yoshi') ~ 'medium',
      TRUE ~ NA_character_
    )
  )

# remove unnecessary columns
drivers <-
  drivers[, c(
    "driver",
    "size",
    "weight",
    "ground_speed",
    "acceleration",
    "ground_handling",
    "on_road_traction"
  )]


##:::::::::
##  Karts
##:::::::::

# read csv
karts <- read_delim(
  "CSV/bodies_karts.csv",
  delim = ";",
  escape_double = FALSE,
  col_types = cols(
    Weight = col_integer(),
    Acceleration = col_integer(),
    `On-Road traction` = col_integer(),
    `Off-Road Traction` = col_integer(),
    `Mini-Turbo` = col_integer(),
    `Ground Speed` = col_integer(),
    `Water Speed` = col_integer(),
    `Anti-Gravity Speed` = col_integer(),
    `Air Speed` = col_integer(),
    `Ground Handling` = col_integer(),
    `Water Handling` = col_integer(),
    `Anti-Gravity Handling` = col_integer(),
    `Air Handling` = col_integer()
  ),
  trim_ws = TRUE
)

# clean column names
karts <- janitor::clean_names(karts, "snake")

# keep only necessary columns
colnames(karts)
karts <-
  karts[, c(
    "body",
    "weight",
    "ground_speed",
    "acceleration",
    "ground_handling",
    "on_road_traction"
  )]


##:::::::::::
##  Gliders
##:::::::::::

# read csv
gliders <- read_delim(
  "CSV/gliders.csv",
  delim = ";",
  escape_double = FALSE,
  col_types = cols(
    Weight = col_integer(),
    Acceleration = col_integer(),
    `On-Road traction` = col_integer(),
    `Off-Road Traction` = col_integer(),
    `Mini-Turbo` = col_integer(),
    `Ground Speed` = col_integer(),
    `Water Speed` = col_integer(),
    `Anti-Gravity Speed` = col_integer(),
    `Air Speed` = col_integer(),
    `Ground Handling` = col_integer(),
    `Water Handling` = col_integer(),
    `Anti-Gravity Handling` = col_integer(),
    `Air Handling` = col_integer()
  ),
  trim_ws = TRUE
)

# clean column names
gliders <- janitor::clean_names(gliders, "snake")

# keep only necessary columns
gliders <-
  gliders[, c(
    "glider",
    "weight",
    "ground_speed",
    "acceleration",
    "ground_handling",
    "on_road_traction"
  )]


##:::::::::
##  Tires
##:::::::::

# read csv
tires <- read_delim(
  "CSV/tires.csv",
  delim = ";",
  escape_double = FALSE,
  col_types = cols(
    Weight = col_integer(),
    Acceleration = col_integer(),
    `On-Road traction` = col_integer(),
    `Off-Road Traction` = col_integer(),
    `Mini-Turbo` = col_integer(),
    `Ground Speed` = col_integer(),
    `Water Speed` = col_integer(),
    `Anti-Gravity Speed` = col_integer(),
    `Air Speed` = col_integer(),
    `Ground Handling` = col_integer(),
    `Water Handling` = col_integer(),
    `Anti-Gravity Handling` = col_integer(),
    `Air Handling` = col_integer()
  ),
  trim_ws = TRUE
)

# clean names
tires <- janitor::clean_names(tires, "snake")

# keep only necessary columns
tires <-
  tires[, c(
    "tire",
    "weight",
    "ground_speed",
    "acceleration",
    "ground_handling",
    "on_road_traction"
  )]


##::::::::::::::::::
##  Configurations
##::::::::::::::::::

# rename columns
drivers <- rename_if(drivers, is.numeric, ~ paste0(., "_drivers"))
karts <- rename_if(karts, is.numeric, ~ paste0(., "_karts"))
tires <- rename_if(tires, is.numeric, ~ paste0(., "_tires"))
gliders <- rename_if(gliders, is.numeric, ~ paste0(., "_gliders"))

# Cartesian product
df_all <-
  crossing(drivers, karts, tires, gliders)

# calculate the total weight / speed / acceleration / handling for the configurations
df_summary <- df_all %>%
  mutate(weight = weight_drivers + weight_karts + weight_tires + weight_gliders) %>%
  mutate(speed = ground_speed_drivers + ground_speed_karts + ground_speed_tires + ground_speed_gliders) %>%
  mutate(acceleration = acceleration_drivers + acceleration_karts + acceleration_tires + acceleration_gliders) %>%
  mutate(handling = ground_handling_drivers + ground_handling_karts + ground_handling_tires + ground_handling_gliders) %>%
  mutate(
    traction = on_road_traction_drivers + on_road_traction_karts + on_road_traction_tires + on_road_traction_gliders
  )

# keep only necessary columns
df <-
  df_summary[, c(
    "driver",
    "size",
    "body",
    "tire",
    "glider",
    "weight",
    "speed",
    "acceleration",
    "handling",
    "traction"
  )]

# calculate a score
df <- df %>%
  mutate(score = traction + handling + acceleration + speed)

# minőségi változókat csináljunk
df$score_category <-
  cut(
    df$score,
    breaks = 5,
    labels = c("bad", "low", "medium", "high", "excellent")
  )
df$speed_category <-
  cut(
    df$speed,
    breaks = 5,
    labels = c("bad", "low", "medium", "high", "excellent")
  )
df$acceleration_category <-
  cut(
    df$acceleration,
    breaks = 5,
    labels = c("bad", "low", "medium", "high", "excellent")
  )

# format columns as factor
df$driver <- as.factor(df$driver)
df$size <- as.factor(df$size)
df$body <- as.factor(df$body)
df$tire <- as.factor(df$tire)
df$glider <- as.factor(df$glider)
df$score_category <- as.factor(df$score_category)
df$speed_category <- as.factor(df$speed_category)
df$acceleration_category <- as.factor(df$acceleration_category)


############################################################################
############################################################################
###                                                                      ###
###                              SECTION 2:                              ###
###                      LEÍRÓ STATISZTIKAI ELEMZÉS                      ###
###                                                                      ###
############################################################################
############################################################################

summary(df)

table(df$score)
binnedCounts(df$score)
gyak_tabla <- as.data.frame(binnedCounts(df$score))

# oszlopnév "szépítése"
colnames(gyak_tabla) <- "Gyakorisag"

# kumulált gyakoriságok számítása új oszlopba
gyak_tabla$Kumulalt_Gyak <- cumsum(gyak_tabla$Gyakorisag)

# eredmény megtekintése
gyak_tabla

# gyakorisági tábla bővítése relatív gyakoriságokkal
gyak_tabla$Rel_Gyak <-
  gyak_tabla$Gyakorisag / sum(gyak_tabla$Gyakorisag)

# kumulált relatív gyakoriságok számítása új oszlopba
gyak_tabla$Kumulalt_Rel_Gyak <- cumsum(gyak_tabla$Rel_Gyak)

# eredmény megtekintése
gyak_tabla

gyak_score <- table(df$score)

# módusz
gyak_score[gyak_score == max(gyak_score)]

# medián
median(df$score)

# átlag
mean(df$score)

plot(table(df$score))
hist(df$score)
hist(df$score, breaks = 7)

ggplot(data = df, aes(x = score)) +
  geom_histogram(bins = 23,
                 color = 1,
                 fill = "white") +
  labs(x = "Score", y = "Gyakoriság (db)", title = "Score eloszlása")

summary(df$score)

by(df$score, df$size, summary)
by(df$score, df$driver, summary)

by(df[, c("speed", "acceleration", "score")], df$size, summary)

# szórás
N <- nrow(df)
atlag <- mean(df$score)
szoras <- sqrt(sum((df$score - atlag) ^ 2) / N)
# relativ szórás
szoras / atlag # 0.0876

# szórás sd
sd(df$score)

describe(df$score)

boxplot(df$score)

ggplot(data = df, aes(y = score)) +
  geom_boxplot()

summary(df$size)

############################################################################
############################################################################
###                                                                      ###
###                              SECTION 3:                              ###
###               INTERVALLUMBECSLÉS ÉS HIPOTÉZISVIZSGÁLAT               ###
###                                                                      ###
############################################################################
############################################################################

# Create a sample
set.seed(1995)
df_sample <- df[sample(nrow(df), size = 48, replace = TRUE), ]
mean(df_sample$score)


##::::::::::::::::::::::
##  Intervallumbecslés
##::::::::::::::::::::::

groupwiseMean(score ~ 1, data = df_sample)
groupwiseMean(score ~ 1, data = df_sample, conf = 0.99)

groupwiseMedian(score ~ size, data = df_sample, conf = 0.99)


##::::::::::::::::::::::
##  Hipotézisvizsgálat
##::::::::::::::::::::::

# H0: sokasági átlag >= 42
# H1: sokasági átlag < 42

t.test(df_sample$score, mu = 45, alternative = "less")


############################################################################
############################################################################
###                                                                      ###
###                              SECTION 4:                              ###
###                    KÉTVÁLTOZÓS KAPCSOLATVIZSGÁLAT                    ###
###                                                                      ###
############################################################################
############################################################################


##::::::::::::::::::::::::::::::::::::::::::::
##  Vegyes kapcsolat (minőségi - mennyiségi)
##::::::::::::::::::::::::::::::::::::::::::::

ggplot(data = df, aes(y = score, x = size, fill = size)) +
  geom_boxplot()

aov(score ~ size, data = df)

sd(df$score) ^ 2 * (nrow(df) - 1)

# ssb / sst
233852 / 7336621 * 100
sqrt(233852 / 7336621)

oneway.test(score ~ size, data = df, var.equal = FALSE)


##:::::::::::::::::::::::::::::::::::::::::::::::
##  Asszociációs kapcsolat (minőségi - minőségi)
##:::::::::::::::::::::::::::::::::::::::::::::::

ggplot(data = df, aes(x = speed_category, fill = size)) +
  geom_bar()

ggplot(data = df, aes(x = speed_category, fill = size)) +
  geom_bar(position = "fill")

round(prop.table(table(df[, c("speed_category", "size")]), 1) * 100, 1)


## Heatmap
# Create a contingency table
data_table <- table(df$speed_category, df$size)

# Convert the table to a data frame
data_table_df <- as.data.frame(data_table)

# Create a ggplot object
ggplot(data_table_df, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(name = "Count",
                      low = "white",
                      high = "gray") +
  geom_text(aes(label = Freq), color = "black", size = 3) +
  ggtitle("Heatmap")


## Cramer
questionr::cramer.v(table(df[, c("speed_category", "size")])) # 0.5663875
# 0.3 <= Cramer <= 0.7 –> közepes kapcsolat


## Khi-négyzet
# H0:A kapcsolat a sokaságban nem szignifikáns, azaz Cramer-együttható = 0 a sokaságban
# H1:A kapcsolat a sokaságban szignifikáns, azaz Cramer-együttható > 0 a sokaságban

chisq.test(table(df[, c("speed_category", "size")]))

# ellenőrizzük a feltételt
table(df[, c("speed_category", "size")])
# mivel nincsen mindenhol meg a legalább 5 elem, nem végezhető el


##:::::::::::::::::::::::::::::::::::::::::::::::::::
##  Korrelációs kapcsolat (mennyiségi - mennyiségi)
##:::::::::::::::::::::::::::::::::::::::::::::::::::


## Heatmap
ggplot(as.data.frame(table(df$speed, df$acceleration)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(name = "Count",
                      low = "white",
                      high = "gray") +
  geom_text(aes(label = Freq), color = "black", size = 2) +
  ggtitle("Heatmap")


## Korreláció
cor(df$speed, df$acceleration)


## Pont diagram
ggplot(data = df, aes(x = speed, y = acceleration)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "Configuration: Speed vs. Acceleration", x = "Speed", y = "Acceleration")


df_freq <- as.data.frame(table(df$speed, df$acceleration))
df_without_0s <- df_freq %>%
  filter(Freq != 0)

ggplot(df_without_0s, aes(x = Var1, y = Var2, size = Freq)) +
  geom_point() +
  labs(title = "Configuration: Speed vs. Acceleration", x = "Speed", y = "Acceleration")

## A determinációs együttható
cor(df$speed, df$acceleration) ^ 2

lm(speed ~ acceleration, data = df)

# RegModell <- lm(acceleration ~ speed, data = df)
# df$becsült_gyorsulas <- predict(RegModell, newdata = df)
# head(df)

# A regressziós egyenes együtthatóinak hipotézsvizsgálata
summary(RegModell)

## Korrelációs kapcsolat egy minőségi változó szerint megbontva
ggplot(data = df, aes(x = speed, y = acceleration, color = size)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "Configuration: Speed vs. Acceleration", x = "Speed", y = "Acceleration")

df$size <- relevel(df$size, ref = "medium")
summary(lm(acceleration ~ speed + size * speed, data = df))
