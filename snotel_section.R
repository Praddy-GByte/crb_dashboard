column(width = 6,
       box(width = 12, title = "SNOTEL SWE",
           plotlyOutput("snotel_trend_plot", height = "400px")
       )
)
) 