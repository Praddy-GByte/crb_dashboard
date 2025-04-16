# Load required packages
library(rsconnect)

# Set account info
rsconnect::setAccountInfo(
  name = Sys.getenv("SHINYAPPS_NAME"),
  token = Sys.getenv("SHINYAPPS_TOKEN"),
  secret = Sys.getenv("SHINYAPPS_SECRET")
)

# Deploy the application
rsconnect::deployApp(
  appDir = ".",
  appName = "crb_dashboard",
  appTitle = "Colorado River Basin Dashboard",
  account = Sys.getenv("SHINYAPPS_NAME"),
  forceUpdate = TRUE,
  launch.browser = FALSE
) 