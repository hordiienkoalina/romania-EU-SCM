# Load necessary libraries
library(Synth)
library(readr)
library(dplyr)
library(tidyr)

# Set the file paths for the data
exports_file <- "exports.csv"
govt_expenditure_file <- "govt-expenditure.csv"
gfcf_file <- "gross-fixed capital-formation.csv"
fdi_file <- "fdi-net-inflows.csv"
inflation_file <- "inflation-consumer-prices.csv"
unemployment_file <- "unemployment.csv"
gdp_file <- "gdp.csv"

# Load the data
exports <- read_csv(exports_file)
govt_expenditure <- read_csv(govt_expenditure_file)
gfcf <- read_csv(gfcf_file)
fdi <- read_csv(fdi_file)
inflation <- read_csv(inflation_file)
unemployment <- read_csv(unemployment_file)
gdp_growth <- read_csv(gdp_file)

# Define the donor pool countries
donor_countries <- c("Bulgaria", "Czechia")

# Function to filter and clean each dataset for the donor pool
clean_and_filter_data <- function(data, countries) {
  data %>%
    select(`Country Name`, starts_with("19"), starts_with("20")) %>%
    filter(`Country Name` %in% c("Romania", countries)) %>%
    mutate(across(starts_with("19") | starts_with("20"), ~ as.numeric(.)))
}

# Clean and filter each dataset
exports_filtered <- clean_and_filter_data(exports, donor_countries)
govt_expenditure_filtered <- clean_and_filter_data(govt_expenditure, donor_countries)
gfcf_filtered <- clean_and_filter_data(gfcf, donor_countries)
fdi_filtered <- clean_and_filter_data(fdi, donor_countries)
inflation_filtered <- clean_and_filter_data(inflation, donor_countries)
unemployment_filtered <- clean_and_filter_data(unemployment, donor_countries)
gdp_growth_filtered <- clean_and_filter_data(gdp_growth, donor_countries)

# Create a numeric identifier for the countries
country_codes <- data.frame(
  `Country Name` = c("Romania", donor_countries),
  Country_Code = 1:(length(donor_countries) + 1)
)

# Prepare the data for SCM (reshaping to long format)
prepare_scm_data <- function(df, outcome_var) {
  df %>%
    pivot_longer(cols = starts_with("19") | starts_with("20"), 
                 names_to = "Year", values_to = outcome_var) %>%
    mutate(Year = as.numeric(Year),
           across(all_of(outcome_var), ~ as.numeric(.)))
}

# Reshape all datasets
exports_long <- prepare_scm_data(exports_filtered, "Exports")
govt_expenditure_long <- prepare_scm_data(govt_expenditure_filtered, "Govt_Expenditure")
gfcf_long <- prepare_scm_data(gfcf_filtered, "GFCF")
fdi_long <- prepare_scm_data(fdi_filtered, "FDI_Net_Inflows")
inflation_long <- prepare_scm_data(inflation_filtered, "Inflation")
unemployment_long <- prepare_scm_data(unemployment_filtered, "Unemployment")
gdp_growth_long <- prepare_scm_data(gdp_growth_filtered, "GDP_Growth")

# Rename 'Country.Name' to 'Country Name' in the country_codes dataframe
country_codes <- country_codes %>%
  rename(`Country Name` = `Country.Name`)

# Merge the covariates into a single dataframe
merged_data <- gdp_growth_long %>%
  left_join(exports_long, by = c("Country Name", "Year")) %>%
  left_join(govt_expenditure_long, by = c("Country Name", "Year")) %>%
  left_join(gfcf_long, by = c("Country Name", "Year")) %>%
  left_join(fdi_long, by = c("Country Name", "Year")) %>%
  left_join(inflation_long, by = c("Country Name", "Year")) %>%
  left_join(unemployment_long, by = c("Country Name", "Year"))

# Create a complete panel with all combinations of countries and years
years <- 1993:2023
full_panel <- expand.grid(`Country Name` = c("Romania", donor_countries), Year = years)

# Merge the full panel with the merged data
merged_data <- full_panel %>%
  left_join(merged_data, by = c("Country Name", "Year"))

# Add country codes to the merged data
merged_data <- merged_data %>%
  left_join(country_codes, by = "Country Name") %>%
  mutate(Country_Code = as.numeric(Country_Code))

# Ensure all relevant variables are numeric
merged_data <- merged_data %>%
  mutate(
    Govt_Expenditure = as.numeric(Govt_Expenditure),
    GFCF = as.numeric(GFCF),
    FDI_Net_Inflows = as.numeric(FDI_Net_Inflows),
    Inflation = as.numeric(Inflation),
    Unemployment = as.numeric(Unemployment),
    GDP_Growth = as.numeric(GDP_Growth)
  )

# Ensure merged_data is a data frame (not a tibble)
merged_data <- as.data.frame(merged_data)

# Verify that the panel is balanced
# Each country should have the same number of observations
table(merged_data$`Country Name`)

# Check for missing data
anyNA(merged_data)

# Run the dataprep function with GDP growth as the dependent variable
dataprep.out <- dataprep(
  foo = merged_data,
  predictors = c("Govt_Expenditure", 
                 "GFCF", 
                 "FDI_Net_Inflows",
                 "Inflation",
                "Unemployment"
                 ),
  predictors.op = "mean",
  dependent = "GDP_Growth",
  unit.variable = "Country_Code",
  time.variable = "Year",
  treatment.identifier = 1,  # Romania's country code
  controls.identifier = 2:(length(donor_countries) + 1),  # Donor pool countries
  time.predictors.prior = c(1993:2006),
  time.optimize.ssr = c(1993:2006),
  unit.names.variable = "Country Name",
  time.plot = c(1993:2023)
)

# Run the synthetic control analysis
synth.out <- synth(dataprep.out)

# Plot Real vs Synthetic Romania
path.plot(dataprep.res = dataprep.out, synth.res = synth.out, Ylab = "GDP (current US$)", Xlab = "Year")

# Add vertical lines marking significant events
abline(v = 2007, col = "black", lty = 2, lwd = 2)  # Romania's EU accession year