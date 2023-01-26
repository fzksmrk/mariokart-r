# Set Working Directory
setwd("/Users/markfazekas/Documents/00_Code/R/HAZIDOLGOZAT/mariokart-r")

# Install Packages
# install.packages("janitor")

# Use Packages
library(readr)
library(janitor)
library(dplyr)
library(stringr)
library(ggplot2)
library(psych)
library(tidyr)
library(RcmdrMisc)
library(rcompanion)

# ======== DRIVERS

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

drivers <- janitor::clean_names(drivers, "snake")

drivers$driver <- gsub("[()]", "", drivers$driver)
# drivers <- drivers %>%
#   group_by_if(is.numeric) %>% #group by all numeric columns
#   summarise_at(vars(-one_of(names(drivers)[is.numeric(drivers)])), paste, collapse = ", ") %>% #concatenate the text variable
#   summarise_all(first) #keep the first value of numeric columns
# drivers <- drivers %>%
#   mutate(number_of_recs = str_count(driver, ",") + 1)

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

# ======== KARTS

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

karts <- janitor::clean_names(karts, "snake")

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

# ======== GLIDERS

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

gliders <- janitor::clean_names(gliders, "snake")

colnames(gliders)
gliders <-
  gliders[, c(
    "glider",
    "weight",
    "ground_speed",
    "acceleration",
    "ground_handling",
    "on_road_traction"
  )]

# ======== TIRES

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

tires <- janitor::clean_names(tires, "snake")

colnames(tires)
tires <-
  tires[, c(
    "tire",
    "weight",
    "ground_speed",
    "acceleration",
    "ground_handling",
    "on_road_traction"
  )]



# ====================
# Combine the 4 dfs
# ====================

# rename columns
drivers <- rename_if(drivers, is.numeric, ~ paste0(., "_drivers"))
karts <- rename_if(karts, is.numeric, ~ paste0(., "_karts"))
tires <- rename_if(tires, is.numeric, ~ paste0(., "_tires"))
gliders <- rename_if(gliders, is.numeric, ~ paste0(., "_gliders"))

df_all <-
  crossing(drivers, karts, tires, gliders)

colnames(df_all)

df_summary <- df_all %>%
  mutate(weight = weight_drivers + weight_karts + weight_tires + weight_gliders) %>%
  mutate(speed = ground_speed_drivers + ground_speed_karts + ground_speed_tires + ground_speed_gliders) %>%
  mutate(acceleration = acceleration_drivers + acceleration_karts + acceleration_tires + acceleration_gliders) %>%
  mutate(handling = ground_handling_drivers + ground_handling_karts + ground_handling_tires + ground_handling_gliders) %>%
  mutate(
    traction = on_road_traction_drivers + on_road_traction_karts + on_road_traction_tires + on_road_traction_gliders
  )

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

df <- df %>%
  mutate(score = traction + handling + acceleration + speed)

df$driver <- as.factor(df$driver)
df$size <- as.factor(df$size)
df$body <- as.factor(df$body)
df$tire <- as.factor(df$tire)
df$glider <- as.factor(df$glider)

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

table(df$score)


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

df$score[(df$score > 53.5)]

summary(df$size)

#sample
set.seed(1995)
df_sample <- df[sample(nrow(df), size = 48, replace = TRUE),]
mean(df_sample$score)

groupwiseMean(score ~ 1, data = df_sample)
groupwiseMean(score ~ 1, data = df_sample, conf = 0.99)

groupwiseMedian(score ~ size, data = df_sample, conf = 0.99)

  # speed_weight_plot <-
#   ggplot(data = df, aes(x = weight, y = speed)) + geom_point() + geom_smooth() + labs(title = "Karts: Weight vs. Speed", x = "Weight", y = "Speed Score")
# 
# score_weight_plot <-
#   ggplot(data = df, aes(x = weight, y = score)) + geom_point() + geom_smooth() + labs(title = "Karts: Weight vs. Score", x = "Weight", y = "Score")
