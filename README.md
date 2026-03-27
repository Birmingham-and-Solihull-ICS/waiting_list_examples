
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Waiting list examples and communication aids

Currently contains:

- `example_of_sim.R` - example sim showing my simulation route to
  estimate them simulate a waiting list.
- `waiting_times_distribution.R` - plot showing the proportion of
  compliant patients at 18weeks, given different average waits.
- `WL_factor_vs_probability_compliance` - Shiny app, showing how
  `factor` argument interacts with % compliance, and calculates a target
  queue size.

## Exponential Factor Compliance App

Access the app here:
<https://chrismainey.shinyapps.io/wl_factor_vs_probability_compliance/>

This repository contains an interactive Shiny application that
visualises the relationship between compliance (expressed as a
percentage) and the exponential distribution factor required to meet a
waiting list target.

The app uses:

- **ggplot2** for plotting
- **plotly** for interactive graph features
- **BSOLutils** for Birmingham & Solihull ICB theme and colour styling
- **shiny** for the reactive interface

------------------------------------------------------------------------

### Features

- Interactive compliance input (as a percentage)
- Dynamic exponential factor calculation using `qexp()`
- Interactive plotly-based visualisation with tooltips
- Display of:
  - Selected compliance (%)
  - Corresponding exponential factor
- Customised ICB colour palette and theme
- Enlarged graph area and refined layout for readability

------------------------------------------------------------------------

### Running the App Locally

Clone the repository:

``` bash
git clone https://github.com/Birmingham-and-Solihull-ICS/waiting_list_examples
cd waiting_list_examples/R

# Run the app from command line with:
Rscript -e "shiny::runApp('shiny_wl_factor.R', launch.browser = TRUE)"
```

or clone within RStudio, Positron or similar and run interactively. You
can view the deployed app at:
<https://chrismainey.shinyapps.io/wl_factor_vs_probability_compliance/>

This repository is dual licensed under the [Open Government
v3](%5Bhttps://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/)
& MIT. All code and outputs are subject to Crown Copyright.
