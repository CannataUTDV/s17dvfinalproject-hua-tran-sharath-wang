# server.R
library(plotly)
library(RColorBrewer)
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)
require(tidyr)
require(leaflet)
require(plotly)

shinyServer(function(input, output) { 
  online1 = reactive({input$rb1})
  AADR_Low = reactive({input$AADR1})     
  AADR_Medium = reactive({input$AADR2})
  
  # These widgets are for the Barcharts tab.
  online2 = reactive({input$rb2})
  
  online3 = reactive({input$rb3})
  
# Begin SNT1 Tab WORKS-----------------------------------------------

  df1 <- eventReactive(input$click1, {
    query(
      data.world(propsfile = "www/.data.world"),
      dataset="ninaxhua/s-17-dv-final-project",
      query="select Death.State as State,
      Death.cause as Cause_of_Death,
      sum(Death.AADR) as sum_AADR,
      
      case
      when sum(Death.AADR) < ? then '03 Low'
      when sum(Death.AADR) < ? then '02 Medium'
      else '01 High'
      end as AADR_HML
      
      from Death
      
      group by Death.State, Death.cause",
      queryParameters = list(AADR_Low(), AADR_Medium())
    ) # %>% View()
  })
  output$data1 <- renderDataTable({DT::datatable(df1(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$plot1 <- renderPlot({ggplot(df1()) + 
      theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=10, hjust=0.5)) +
      geom_text(aes(x=Cause_of_Death, y=State, label=sum_AADR), size=3) +
      geom_tile(aes(x=Cause_of_Death, y=State, fill=AADR_HML), alpha=0.50)
  })
# End SNT1 Tab _________________________________________________
# Begin SNT2 Tab WORKS---------------------------------------------------------------
  df8 <- eventReactive(input$click8, {
    query(
      data.world(propsfile = "www/.data.world"),
      dataset="ninaxhua/s-17-dv-final-project", type="sql",
      query="select Death.State,
      sum(AADR) as sum_AADR,
      (sum(Death.m_bs)/sum(Death.`edu.males`) + sum(Death.f_bs)/sum(Death.`edu.females`))/2 as `Percent BS`
      
      from Death
      group by Death.State"
    )
  })
  output$data8 <- renderDataTable({DT::datatable(df8(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$plot8 <- renderPlot({ggplot(df8()) +
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      geom_point(aes(x = `Percent BS`, y = sum_AADR, colour = State)) +
      labs(x = "Education Level", y = "Death Rate") +
      expand_limits(y = 0, x = 0)
    #facet_wrap(~Gender)
  })
# End SNT2 Tab ___________________________________________________________
# Begin NH1 Tab WORKS ------------------------------------
  df2 <- eventReactive(input$click2, {
    print("Getting from data.world")
    tdf2_1 = query(
      data.world(propsfile = "www/.data.world"),
      dataset = "ninaxhua/s-17-dv-final-project", type = "sql",
      query = "select Death.year,
                Death.cause,
                sum(Death.AADR)
                
                from Death
                where Death.cause != 'All Causes'
                group by Death.year, Death.cause"
    )
    tdf2_2 = tdf2_1 %>% dplyr::group_by(year) %>% dplyr::summarize(window_avg_AADR = mean(AADR))
    dplyr::inner_join(tdf2_1, tdf2_2, by = "year")
  })
  
  output$barchartData2 <- renderDataTable({DT::datatable(df2(),
                                                         rownames = FALSE,
                                                         extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
  
  output$barchartPlot2_All <- renderPlot({ggplot(df2(), aes(x=cause, y=AADR)) +
      scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=1, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=1, hjust=1)) +
      geom_bar(stat = "identity") + 
      facet_wrap(~year, ncol=1) + 
      labs(x = "Cause of Death", y = "Death Rate") +
      coord_flip() + 
      # Add sum_sales, and (sum_sales - window_avg_sales) label.
      geom_text(mapping=aes(x=cause, y=AADR, label=round(AADR)),colour="black", hjust=-.5, size = 1) +
      geom_text(mapping=aes(x=cause, y=AADR, label=round(AADR - window_avg_AADR)),colour="blue", hjust=-2, size = 1) +
      # Add reference line with a label.
      geom_hline(aes(yintercept = round(window_avg_AADR)), color="red") +
      geom_text(aes( -1, window_avg_AADR, label = window_avg_AADR, vjust = -.5, hjust = -.25), color="red", size = 1)
  })
  output$barchartPlot2_Zoom <- renderPlot({ggplot(df2(), aes(x=cause, y=AADR)) +
      scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=10, hjust=1)) +
      geom_bar(stat = "identity") + 
      facet_wrap(~year, ncol=2) + 
      labs(x = "Cause of Death", y= "Death Rate") +
      coord_flip() + 
      # Add sum_sales, and (sum_sales - window_avg_sales) label.
      geom_text(mapping=aes(x=cause, y=AADR, label=round(AADR)),colour="black", hjust=-.5) +
      geom_text(mapping=aes(x=cause, y=AADR, label=round(AADR - window_avg_AADR)),colour="blue", hjust=-2) +
      # Add reference line with a label.
      geom_hline(aes(yintercept = round(window_avg_AADR)), color="red") +
      geom_text(aes( -1, window_avg_AADR, label = window_avg_AADR, vjust = -.5, hjust = -.25), color="red")
  })
#End NH1 Tab ____________________________
# Begin NH2 Tab WORKS ------------------------------------
  df3 <- eventReactive(input$click3, {
          query(
                  data.world(propsfile = "www/.data.world"),
                  dataset = "ninaxhua/s-17-dv-final-project", type = "sql",
                  query = "select Death.State, sum(Death.f_bs)/sum(Death.`edu.females`) as f_bs_frac 
from Death group by Death.State having sum(Death.f_bs)/sum(Death.`edu.females`) between 0.15 and 0.25"
          )
          
  })
  
  output$barchartData3 <- renderDataTable({DT::datatable(df3(),
                                                         rownames = FALSE,
                                                         extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
  
  output$barchartPlot3 <- renderPlot({ggplot(df3(), aes(x=State, y=f_bs_frac)) +
                  scale_y_continuous(labels = scales::comma) + # no scientific notation
                  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
                  theme(axis.text.y=element_text(size=12, hjust=1)) +
                  geom_col(aes(x = State, y = f_bs_frac), fill = "navy")
  })  

  df4 <- eventReactive(input$click4, {
          query(
                  data.world(propsfile = "www/.data.world"),
                  dataset = "ninaxhua/s-17-dv-final-project", type = "sql",
                  query = "select Death.State, sum(Death.f_bs)/sum(Death.`edu.females`) as f_bs_frac 
                  from Death group by Death.State 
                  having sum(Death.f_bs)/sum(Death.`edu.females`) < 0.15"
          )
          
  })
  
  output$barchartData4 <- renderDataTable({DT::datatable(df4(),
                          rownames = FALSE,
                          extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
  
  output$barchartPlot4 <- renderPlot({ggplot(df4(), aes(x=State, y=f_bs_frac)) +
                  scale_y_continuous(labels = scales::comma) + # no scientific notation
                  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
                  theme(axis.text.y=element_text(size=12, hjust=1)) +
                  geom_col(aes(x = State, y = f_bs_frac), fill = "navy")
  })  
  #End NH2 Tab ____________________________
  
# Begin NH3 Tab ------------------------------------
  df5 <- eventReactive(input$click5, {
          query(
                  data.world(propsfile = "www/.data.world"),
                  dataset = "ninaxhua/s-17-dv-final-project", type = "sql",
                  query = "select Death.State, sum(Death.m_bs)/sum(Death.`edu.males`) as m_bs_frac 
                  from Death group by Death.State 
                  having sum(Death.m_bs)/sum(Death.`edu.males`) between 0.15 and 0.25"
          )
          
  })
  
  output$barchartData5 <- renderDataTable({DT::datatable(df5(),
                           rownames = FALSE,
                           extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
  
  output$barchartPlot5 <- renderPlot({ggplot(df5(), aes(x=State, y=m_bs_frac)) +
                  scale_y_continuous(labels = scales::comma) + # no scientific notation
                  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
                  theme(axis.text.y=element_text(size=12, hjust=1)) +
                  geom_col(aes(x = State, y = m_bs_frac), fill = "plum4")
  })  
  
  df6 <- eventReactive(input$click6, {
          query(
                  data.world(propsfile = "www/.data.world"),
                  dataset = "ninaxhua/s-17-dv-final-project", type = "sql",
                  query = "select Death.State, sum(Death.m_bs)/sum(Death.`edu.males`) as m_bs_frac 
                  from Death group by Death.State 
                  having sum(Death.m_bs)/sum(Death.`edu.males`) < 0.15"
          )
          
  })
  
  output$barchartData6 <- renderDataTable({DT::datatable(df6(),
                          rownames = FALSE,
                          extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
  
  output$barchartPlot6 <- renderPlot({ggplot(df6(), aes(x=State, y=m_bs_frac)) +
                  scale_y_continuous(labels = scales::comma) + # no scientific notation
                  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
                  theme(axis.text.y=element_text(size=12, hjust=1)) +
                  geom_col(aes(x = State, y = m_bs_frac), fill = "plum4")
  })  
  #End NH3 Tab ____________________________  
  
#Begin SNT3 Tab----------------------
  df7 <- eventReactive(input$click7, {
    print("Getting from data.world")
    tdf7 = query(
      data.world(propsfile = "www/.data.world"),
      dataset = "ninaxhua/s-17-dv-final-project", type = "sql",
      query = "select Death.State,
                  ((sum(Death.f_bs)+sum(Death.m_bs))/(sum(Death.`edu.males`)+sum(Death.`edu.females`))*100) as PercentBS,
                  sum(Death.AADR) as AADR
                from Death
                group by State"
    )
    tdf7$hover = with(tdf7, paste(State, '<br>', "Education Level", round(PercentBS, 2)))
    tdf7$hover2 = with(tdf7, paste(State, '<br>', "Death Rate", AADR))
    tdf7
    
  })
  
  output$mapData7 <- renderDataTable({DT::datatable(df7(),
                           rownames = FALSE,
                           extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
  
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = TRUE,
    lakecolor = toRGB('white')
  )
  
  output$map1 <- renderPlotly({plot_geo(df7(), locationmode = 'USA-states') %>%
      add_trace(z= ~PercentBS, text = ~hover, color = ~PercentBS, colors = brewer.pal(3, "RdYlGn"), locations = ~State) %>%
      colorbar(title = "Education Level") %>%
      layout(title = "Education Level - LOW (Red) to HIGH (Green)", geo = g)
  })
  
  output$map2 <- renderPlotly({plot_geo(df7(), locationmode = 'USA-states') %>%
      add_trace(z= ~AADR, text = ~hover2, color = ~AADR, colors = rev(brewer.pal(3, "RdYlGn")), locations = ~State) %>%
      colorbar(title = "Death Rate") %>%
      layout(title = "Death Rate - HIGH (Red) to LOW (Green)", geo = g)
  })
  
  #End SNT3 Tab ___________________________
  
  #Begin NH4 Tab----------------------
  df9 <- eventReactive(input$click9, {
          print("Getting from data.world")
          tdf9 = query(
                  data.world(propsfile = "www/.data.world"),
                  dataset = "ninaxhua/s-17-dv-final-project", type = "sql",
                  query = "select Death.cause, Death.amt_death, Death.State, Death.year 
                  from Death
                  where Death.cause != 'All Causes'"
          )
          tdf9

  })
  
  output$boxData9 <- renderDataTable({DT::datatable(df9(),
                                                    rownames = FALSE,
                                                    extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
  
  output$boxplot9 <- renderPlot({
                ggplot(df9()) + 
                  geom_boxplot(aes(x=cause, y=amt_death)) +
                  theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5)) +
                  coord_cartesian(ylim = c(0,80000))
  })
  
  #End NH4 Tab ___________________________
# Begin Histgram Tab ------------------------------------------------------------------
  dfh1 <- eventReactive(input$click10, {
    print("Getting from data.world")
    query(
      data.world(propsfile = "www/.data.world"),
      dataset="ninaxhua/s-17-dv-final-project", type="sql",
      query="select AADR, cause, State, year
      from Death
      where cause = 'All Causes' and year = 2010")
  })
  
  output$histogramData1 <- renderDataTable({DT::datatable(dfh1(), rownames = FALSE,
                                                          extensions = list(Responsive = TRUE, 
                                                                            FixedHeader = TRUE)
  )
  })
  
  output$histogramPlot1 <- renderPlot({ggplot(dfh1()) +
    geom_histogram(aes(x=AADR)) +
    theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))
  })
  # End Histogram Tab ___________________________________________________________
})


