# Analysis Tab Code
# Add this to your app.R file within the tabItems() section

tabItem(tabName = "analysis",
        fluidRow(
          box(title = "Historical Trends Analysis", width = 12,
              tabsetPanel(
                tabPanel("SMAP Surface",
                         img(src = "images/smap_surface_trend.png", 
                             width = "100%", height = "auto")),
                tabPanel("SMAP Root Zone",
                         img(src = "images/smap_rootzone_trend.png", 
                             width = "100%", height = "auto")),
                tabPanel("GRACE TWS",
                         img(src = "images/grace_trend.png", 
                             width = "100%", height = "auto")),
                tabPanel("SNOTEL Trends",
                         img(src = "images/snotel_trend.png",
                             width = "100%", height = "auto")),
                tabPanel("Combined Analysis",
                         img(src = "images/combined_trends.png", 
                             width = "100%", height = "auto")),
                tabPanel("Summary Statistics",
                         dataTableOutput("summaryStats"))
              )
          )
        )
) 