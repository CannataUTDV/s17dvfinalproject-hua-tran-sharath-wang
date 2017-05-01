# server.R
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)
require(tidyr)
require(leaflet)

shinyServer(function(input, output) { 
  
#Begin SNT1 Tab ------------------------------------
  df1 <- eventReactive(input$click1, {
    print("Getting from data.world")
    tdf1_1 = query(
      data.world(propsfile = "www/.data.world"),
      dataset = "ninaxhua/s-17-dv-project-6", type = "sql",
      query = "select Death.State,
                Death.cause,
                sum(Death.AADR) as AADR
            
                from Death
                where Death.cause != 'All Causes'
                group by Death.State, Death.cause"
    )
    tdf1_2 = tdf1_1 %>% group_by(State) %>% summarize(window_avg_AADR = mean(AADR))
    dplyr::inner_join(tdf1_1, tdf1_2, by = "State")
  })
  
  output$barchartData1 <- renderDataTable({DT::datatable(df1(),
                         rownames = FALSE,
                         extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
  
  output$barchartPlot1 <- renderPlot({ggplot(df1(), aes(x=cause, y=AADR)) +
      scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=1)) +
      geom_bar(stat = "identity") + 
      facet_wrap(~State, ncol=1) + 
      coord_flip() + 
      # Add sum_sales, and (sum_sales - window_avg_sales) label.
      geom_text(mapping=aes(x=cause, y=AADR, label=round(AADR)),colour="black", hjust=-.5) +
      geom_text(mapping=aes(x=cause, y=AADR, label=round(AADR - window_avg_AADR)),colour="blue", hjust=-2) +
      # Add reference line with a label.
      geom_hline(aes(yintercept = round(window_avg_AADR)), color="red") +
      geom_text(aes( -1, window_avg_AADR, label = window_avg_AADR, vjust = -.5, hjust = -.25), color="red")
  })
#End SNT1 Tab ____________________________
  
#Begin NH1 Tab ------------------------------------
  df2 <- eventReactive(input$click2, {
    print("Getting from data.world")
    tdf2_1 = query(
      data.world(propsfile = "www/.data.world"),
      dataset = "ninaxhua/s-17-dv-project-6", type = "sql",
      query = "select Death.year,
                Death.cause,
                sum(Death.AADR)
                
                from Death
                where Death.cause != 'All Causes'
                group by Death.year, Death.cause"
    )
    tdf2_2 = tdf2_1 %>% group_by(year) %>% summarize(window_avg_AADR = mean(AADR))
    dplyr::inner_join(tdf2_1, tdf2_2, by = "year")
  })
  
  output$barchartData2 <- renderDataTable({DT::datatable(df2(),
                                                         rownames = FALSE,
                                                         extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
  
  output$barchartPlot2 <- renderPlot({ggplot(df2(), aes(x=cause, y=AADR)) +
      scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=1)) +
      geom_bar(stat = "identity") + 
      facet_wrap(~year, ncol=1) + 
      coord_flip() + 
      # Add sum_sales, and (sum_sales - window_avg_sales) label.
      geom_text(mapping=aes(x=cause, y=AADR, label=round(AADR)),colour="black", hjust=-.5) +
      geom_text(mapping=aes(x=cause, y=AADR, label=round(AADR - window_avg_AADR)),colour="blue", hjust=-2) +
      # Add reference line with a label.
      geom_hline(aes(yintercept = round(window_avg_AADR)), color="red") +
      geom_text(aes( -1, window_avg_AADR, label = window_avg_AADR, vjust = -.5, hjust = -.25), color="red")
  })
#End NH1 Tab ____________________________
  #Begin NH2 Tab ------------------------------------
  df3 <- eventReactive(input$click3, {
          query(
                  data.world(propsfile = "www/.data.world"),
                  dataset = "ninaxhua/s-17-dv-project-6", type = "sql",
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
                  dataset = "ninaxhua/s-17-dv-project-6", type = "sql",
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
  
  #Begin NH3 Tab ------------------------------------
  df5 <- eventReactive(input$click5, {
          query(
                  data.world(propsfile = "www/.data.world"),
                  dataset = "ninaxhua/s-17-dv-project-6", type = "sql",
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
                  dataset = "ninaxhua/s-17-dv-project-6", type = "sql",
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
  
})


