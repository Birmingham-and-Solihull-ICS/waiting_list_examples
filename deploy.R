# ---------------------------------------------------------
# deploy.R — Deployment script for shinyapps.io
# ---------------------------------------------------------

# Install once (comment out after first run)
# install.packages("rsconnect")

library(rsconnect)

# Set your account info (replace with your shinyapps.io details)
rsconnect::setAccountInfo(
    name   = "chrismainey",
    token  = Sys.getenv("shiny_token"),
    secret = Sys.getenv("shiny_secret")
)

# Deploy the app in the current directory
rsconnect::deployApp("./WL_factor_vs_probability_compliance")

# After deployment, your app will be available at:
# https://chrismainey.shinyapps.io/wl_factor_vs_probability_compliance/