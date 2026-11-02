# Climate and Breeding Phenology in European Chaffinches

## Overview

This project investigates relationships between environmental conditions and breeding phenology in common chaffinches (*Fringilla coelebs*), using historical breeding records and daily climate observations.

The analysis examines whether temperature and rainfall during different periods of the year are associated with variation in laying dates, and identifies the environmental windows that best explain annual differences in breeding timing.

It also explores how laying dates might change under hypothetical warming scenarios, using relationships estimated from the historical data.

All analyses are conducted in R using publicly available ecological datasets.

## Data

The project uses two datasets included in the `climwin` R package:

- **Chaff:** Annual mean laying dates for common chaffinches across 47 years.
- **ChaffClim:** Daily maximum temperature and rainfall records covering 1965–2012.

The datasets provide an opportunity to investigate how environmental variation relates to reproductive timing over several decades.

The data are loaded directly from the R package, so no external downloads are required.

## Analyses

### Breeding phenology

Annual laying dates are examined to identify long-term temporal patterns in breeding timing.

A linear regression is fitted to assess whether mean laying dates have changed over the study period.

### Environmental cue identification

Monthly temperature and rainfall summaries are calculated for January, February and March.

Linear models are used to examine associations between laying dates and environmental conditions during each month.

Regression coefficients, p-values, R² values and Akaike's Information Criterion (AIC) are used to evaluate the relationships.

### Identification of critical climate windows

Six candidate environmental windows are compared:

- January
- February
- March
- January–February
- February–March
- January–March

For each window, three models are fitted: temperature alone, rainfall alone, and temperature combined with rainfall.

Models are compared using AIC, differences in AIC and model weights to identify the best-supported combination of environmental variables and timing.

### Temporal trends

The best-supported climate model is compared with an alternative that includes year as an additional predictor.

This assesses whether the relationship between climate and laying dates remains after accounting for a linear temporal trend.

### Hypothetical warming scenarios

A temperature and rainfall model is used to explore changes in predicted laying dates under four scenarios:

- Historical mean temperature
- An increase of 1°C
- An increase of 2°C
- An increase of 3°C

Rainfall is held constant at its historical mean.

Predicted laying dates and 95% confidence intervals are calculated for each scenario. Scenarios extending beyond the observed temperature range are identified.

These projections illustrate the sensitivity of breeding phenology to temperature under the fitted statistical relationship, rather than predicting future breeding dates.

### Model diagnostics

Model assumptions are evaluated using residual diagnostic plots, temporal autocorrelation analysis and a Ljung–Box test.

The best-supported climate model is also compared with a null model to evaluate its explanatory value.

## R packages

The analysis uses:

- `climwin` – ecological and climate datasets
- `dplyr` – data preparation and manipulation
- `ggplot2` – data visualisation

## Running the analysis

Install the required packages:

```r
install.packages(c("climwin", "dplyr", "ggplot2"))
```

Run the `wildlife_phenology_climate.R` script in RStudio.

The script automatically loads the datasets, fits the models, generates figures and saves outputs to the `results` and `figures` directories.

## Outputs

The analysis produces:

**Figures**
- Annual laying dates and temporal trends
- Monthly environmental model comparisons
- Comparisons of candidate climate windows
- Temperature and laying date relationships
- Rainfall and laying date relationships
- Predicted laying dates under hypothetical warming scenarios

**Results**
- Monthly model comparison tables
- Climate-window model comparison tables
- Regression coefficients and model summaries
- Warming scenario predictions
- Model residuals and diagnostic information
- Saved model objects and R session information

## Ecological relevance

The timing of reproduction can influence reproductive success, particularly when breeding must coincide with seasonal changes in food availability and environmental conditions.

Identifying climate periods associated with breeding phenology can help researchers understand how wildlife responds to environmental variability.

Climate-window approaches may also be useful for investigating reproductive timing, migration, emergence and other seasonal biological events across different species.

## Limitations

The analysis identifies statistical associations between climate and breeding timing rather than demonstrating causal relationships.

Candidate climate windows are restricted to January–March, and overlapping windows are not statistically independent. Model selection uncertainty should therefore be considered when interpreting the best-supported window.

The models assume linear relationships and do not explicitly account for all potential ecological drivers of breeding phenology.

The warming scenarios assume that the historical temperature–phenology relationship remains unchanged and do not incorporate adaptation, changing food availability or other future environmental changes.

The analysis should be considered exploratory, and model diagnostics should be reviewed before drawing ecological conclusions.

## Data source

Common chaffinch breeding phenology and climate datasets supplied with the `climwin` R package.

Package documentation: https://cran.r-project.org/package=climwin

## Reproducibility

The project uses datasets distributed with an established R package and a single analysis script. Model outputs, figures and session information are saved to support reproducibility.
