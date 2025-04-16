# Define sidebar menu
sidebar_menu <- sidebarMenu(
  menuItem("Spatial Map", tabName = "spatial_map", icon = icon("map")),
  menuItem("VIC Model", tabName = "vic", icon = icon("water")),
  menuItem("SMAP", tabName = "smap", icon = icon("satellite")),
  menuItem("GRACE", tabName = "grace", icon = icon("globe")),
  menuItem("Precipitation Analysis", tabName = "precipitation", icon = icon("cloud-rain")),
  menuItem("Snow Water Equivalent", tabName = "swe", icon = icon("snowflake")),
  menuItem("Soil Moisture", tabName = "soil_moisture", icon = icon("seedling")),
  menuItem("SWE Anomalies", tabName = "swe_anomalies", icon = icon("chart-line")),
  menuItem("Help", tabName = "help", icon = icon("question-circle"))
) 