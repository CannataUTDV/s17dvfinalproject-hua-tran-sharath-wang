# creating chloropleth map
require(plotly)
require(leaflet)
require(rgdal)
require(sf)
# create new columns for percent BS attainment 
df <- df %>% dplyr::group_by(cause) %>% dplyr::filter(AreaName != "United States") %>% dplyr::mutate(f_percent_BS = round((f_bs/edu.females)*100,2), m_percent_BS = round((m_bs/edu.males)*100,2)) %>% dplyr::mutate(hoverF = with(df, paste(AreaName, '<br>', "Female % BS Attainment", f_percent_BS))) %>% dplyr::mutate(hoverM = with(df, paste(AreaName, '<br>', "Male % BS Attainment", m_percent_BS))) 

df$hoverF <- with(df, paste(AreaName, '<br>', "Female % BS Attainment", f_percent_BS))

#give state boundaries a black color
l <- list(color = toRGB("black"), width = 2)

# specify some map projection/options
g <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showlakes = TRUE,
        lakecolor = toRGB('white')
)

f_choropleth <- plot_geo(df, locationmode = 'USA-states') %>%
        add_trace(z= ~f_percent_BS, text = ~hoverF, color = ~f_percent_BS, colors = 'Purples', locations = ~State) %>%
        colorbar(title = "Attainment %") %>%
        layout(title = "Bachelor's Degree Attainment: Females", geo = g)

m_choropleth <- plot_geo(df, locationmode = 'USA-states') %>%
        add_trace(z= ~m_percent_BS, text = ~hoverM, color = ~m_percent_BS, colors = 'Greens', locations = ~State) %>%
        colorbar(title = "Attainment %") %>%
        layout(title = "Bachelor's Degree Attainment: Males", geo = g)
