

function(input, output, session) {

  ############################Trend tab plot###########################
  
  ## filter the data
  # filter the countries
  trend_filter1 <- reactive({
    if(input$plotCountries == "OECD"){
      suicide_rate
    }
    else{
      suicide_rate %>% filter(suicide_rate$Country == input$plotCountries)
    }
  })
  # filter the year
  trend_filter2 <- reactive({
    trend_filter1() %>% filter(trend_filter1()$year >= input$plotSliderYear[1] & trend_filter1()$year <= input$plotSliderYear[2])
  })
  # filter gender 
  trend_filter3 <-reactive({
    trend_filter2()%>% filter(trend_filter2()$sex %in% input$gender,
                              trend_filter2()$age %in% input$ageGroup)
  })


  # plot the year trend line chart
  output$trendPlot <- renderHighchart({
    
    yearly_trend <- trend_filter3() %>% group_by(year) %>% summarize(
      suicides_per_100k = (sum(suicides_no) / sum(population)) * 100000)
    
    highchart() %>% 
      hc_add_series(yearly_trend, hcaes(x = year, y = suicides_per_100k, color = suicides_per_100k), type = "line", name = "suicide rate") %>%
      hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "") %>%
      hc_title(text = "suicides rate by year") %>% 
      hc_xAxis(title = list(text = "year")) %>%
      hc_yAxis(title = list(text = "Suicides per 100K people"),
               allowDecimals = FALSE,
               plotLines = list(list(
                 color = "black", width = 1, dashStyle = "Dash", 
                 value = mean(yearly_trend$suicides_per_100k),
                 label = list(
                              style = list(color = "black", fontSize = 11))))) %>%
      hc_legend(enabled = FALSE) %>% 
      hc_add_theme(hc_theme_flat())
  })
  
  ####information card
  output$vbox <- renderValueBox({
    yearly_trend <- trend_filter3() %>% group_by(year) %>% summarize(
      suicides_per_100k = (sum(suicides_no) / sum(population)) * 100000)
    valueBox(
      "Average Suicide Rate",
      mean(yearly_trend$suicides_per_100k),
      icon = icon("list-alt")
    )
    
    ## plot the line chart by age group
    output$lineTrendAge <- renderHighchart({
      yearly_trend <- trend_filter1() %>% group_by(year, age) %>% summarize(
        suicides_per_100k = (sum(suicides_no) / sum(population)) * 100000)
      highchart() %>% 
        hc_add_series(yearly_trend, hcaes(x = year, y = suicides_per_100k, group = age), type = "line") %>%
        hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", pointFormat = paste("Year: <b>{point.x}</b> <br>","Age: <b>{point.age}</b><br>", "Suicides: <b>{point.y}</b>")) %>%
        hc_title(text = "Suicides by Age") %>% 
        hc_xAxis(title = list(text = "Year")) %>%
        hc_yAxis(title = list(text = "Suicides per 100K people"),
                 allowDecimals = FALSE) %>% 
        hc_add_theme(hc_theme_flat())
    })
    ## plot the line chart by gender
    output$genderTrendLine <- renderHighchart({
      sex_tibble <- trend_filter1() %>% group_by(year, sex) %>% summarize(
        suicides_per_100k = (sum(suicides_no) / sum(population)) * 100000)
      highchart() %>% 
        hc_add_series(sex_tibble, hcaes(x = year, y = suicides_per_100k, group = sex), type = "line") %>%
        hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", pointFormat = paste("Year: <b>{point.x}</b> <br>","Gender: <b>{point.sex}</b><br>", "Suicides: <b>{point.y}</b>")) %>%
        hc_title(text = " Suicides by Gender") %>% 
        
        hc_xAxis(title = list(text = "Year")) %>%
        hc_yAxis(title = list(text = "Suicides per 100K people"),
                 allowDecimals = FALSE
        ) %>% 
        hc_add_theme(hc_theme_flat())
    })
  })
  ############################Size tab plot###########################
  #filter data by year
  size_filter <- reactive({
    suicide_factor %>% filter(suicide_factor$Year >= input$sizePlotSliderYear[1] & suicide_factor$Year <= input$sizePlotSliderYear[2])
  })
  
  output$sizeBar <- renderHighchart({
    # make dataset ready to plot bar graph
    total_size <- size_filter() %>% group_by(Country) %>% summarize(
      suicides_per_100k = (sum(suicides) / sum(population)) * 100000) %>% arrange(desc(suicides_per_100k))

    highchart() %>%
      hc_add_series(total_size, hcaes(x = Country, y = suicides_per_100k, color =rainbow_hcl(35)), type = "bar")  %>% 
      hc_tooltip(borderWidth = 1.5, 
                 pointFormat = paste("Suicides: <b>{point.y}</b>")) %>%
      hc_legend(enabled = FALSE) %>%
      hc_title(text = "Suicides by country") %>% 
      hc_xAxis(categories = total_size$Country, 
               labels = list(step = 1),
               min = 0, max = 25,
               scrollbar = list(enabled = TRUE)) %>%
      hc_yAxis(title = list(text = "Suicides per 100K people")) %>%
      hc_plotOptions(bar = list(stacking = "normal", 
                                pointPadding = 0, groupPadding = 0, borderWidth = 0.5)) %>% 
      hc_add_theme(hc_theme_flat()) 
  })
  
  # output$bubbleTotal <- renderBubbles({
  #   # make dataset ready to plot bar graph
  #   total_size1 <- size_filter() %>% group_by(Country) %>% summarize(
  #     suicides_per_100k = (sum(suicides) / sum(population)) * 100000)
  #   # sort descending
  #   total_size1 <- total_size1[with(total_size1, order(-suicides_per_100k)), ]
  #   bubbles(total_size1$suicides_per_100k, label = total_size1$Country, color = rainbow(35, alpha=NULL) )
  # })
  
  output$mapView <- renderHighchart({
    country <- size_filter() %>% group_by(Country) %>% summarize(
      suicides_per_100k = (sum(suicides) / sum(population)) * 100000)
    highchart() %>%
      hc_add_series_map(worldgeojson, country, value = "suicides_per_100k", joinBy = c('name','Country'))  %>% 
      hc_colorAxis(stops = color_stops()) %>% 
      hc_title(text = "Suicides by Country") %>% 
      hc_tooltip(borderWidth = 1.5, headerFormat = "", valueSuffix = " suicides (per 100K people)") %>% 
      hc_add_theme(hc_theme_flat())
  })
  
  ##get the sub-dataset to plot gender and age
  #filter the data by countries and reform the dataset to plot back-to-back bar chart
  genderAge1 <- reactive({
    if(input$plotGACountries == "OECD"){
      genderAge
    }
    else{
      genderAge %>% filter(genderAge$Country == input$plotGACountries)
    }
  })
  # change the dataset to plot sankey diagram
  output$sankeyGender <- renderSankeyNetwork({
    genderAgeTt <- genderAge1() %>%
      group_by(age, sex) %>%
      summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)
    nodes <- data.frame(
      name=c(as.character(genderAgeTt$age), as.character(genderAgeTt$sex)) %>% 
        unique()
    )
    
    genderAgeTt$IDsource <- match(genderAgeTt$age, nodes$name)-1 
    genderAgeTt$IDtarget <- match(genderAgeTt$sex, nodes$name)-1
    sankeyNetwork(Links = genderAgeTt, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                       Value = "suicide_per_100k", NodeID = "name", fontSize=13, nodePadding=20)
  
  })
  # plot pie chart for age group
  output$pieAge <- renderHighchart({
    pie_age <- genderAge1() %>% group_by(age) %>%
      summarise(suicide_per_100k = round((sum(suicides_no)/sum(population))*100000, 2)) %>%
      arrange(suicide_per_100k)
    highchart() %>% 
      hc_add_series(pie_age, hcaes(x = age, y = suicide_per_100k), type = "pie") %>%
      hc_tooltip(borderWidth = 1.5, headerFormat = "",pointFormat = paste("Suicides: <b>{point.y}</b>") )  %>% 
      hc_plotOptions(pie = list(dataLabels = list(distance = 5, 
                                                  style = list(fontSize = 12)), 
                                size = 250)) 
  })
}