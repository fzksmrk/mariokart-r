



# Set Working Directory
setwd("/Users/markfazekas/Documents/00_Code/R/HAZIDOLGOZAT/mariokart-r")

# Install Packages
install.packages("janitor")

# Use Packages
library(readr)
library(janitor)

# Read CSVs
bodies_karts <- read_delim(
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

# Clean DF
# headers
# https://rdrr.io/cran/janitor/man/clean_names.html
bodies_karts <- janitor::clean_names(bodies_karts, "snake")
drivers <- janitor::clean_names(drivers, "snake")
gliders <- janitor::clean_names(gliders, "snake")
tires <- janitor::clean_names(tires, "snake") 






















