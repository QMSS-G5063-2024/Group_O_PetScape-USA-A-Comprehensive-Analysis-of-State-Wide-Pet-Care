library(readr)
merged_df <- read_csv("merged_df.csv")

library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)
library(maps)
library(shinydashboard)
library(ggplot2)
library(sf)
library(viridis)
library(plotly)
library(RColorBrewer)
library(readr)
library(treemap)
library(htmlwidgets)

states <- data.frame(
  state = c('alabama', 'arizona', 'arkansas', 'california', 'colorado', 'connecticut', 'delaware', 'florida', 'georgia',
            'idaho', 'illinois', 'indiana', 'iowa', 'kansas', 'kentucky', 'louisiana', 'maine', 'maryland',
            'massachusetts', 'michigan', 'minnesota', 'mississippi', 'missouri', 'montana', 'nebraska', 'nevada',
            'new hampshire', 'new jersey', 'new mexico', 'new york', 'north carolina', 'north dakota', 'ohio',
            'oklahoma', 'oregon', 'pennsylvania', 'rhode island', 'south carolina', 'south dakota', 'tennessee',
            'texas', 'utah', 'vermont', 'virginia', 'washington', 'west virginia', 'wisconsin', 'wyoming'),
  latitude = c(32.806671, 34.168219, 34.969704, 36.116203, 39.059811, 41.597782, 39.318523, 27.766279, 33.040619,
               44.240459, 40.349457, 39.849426, 42.011539, 38.526600, 37.668140, 31.169546, 44.693947, 39.063946,
               42.230171, 43.326618, 45.694454, 32.741646, 38.456085, 46.921925, 41.125370, 38.313515, 43.452492,
               40.298904, 34.840515, 42.165726, 35.630066, 47.528912, 40.388783, 35.565342, 44.572021, 40.590752,
               41.680893, 33.856892, 44.299782, 35.747845, 31.054487, 40.150032, 44.045876, 37.769337, 47.400902,
               38.491226, 44.268543, 42.755966),
  longitude = c(-86.791130, -111.930907, -92.373123, -119.681564, -105.311104, -72.755371, -75.507141, -81.686783,
                -83.643074, -114.478828, -88.986137, -86.258278, -93.210526, -96.726486, -84.670067, -91.867805,
                -69.381927, -76.802101, -71.530106, -84.536095, -93.900192, -89.678696, -92.288368, -110.454353,
                -98.268082, -117.055374, -71.563896, -74.521011, -106.248482, -74.948051, -79.806419, -99.784012,
                -82.764915, -96.928917, -122.070938, -77.209755, -71.511780, -80.945007, -99.438828, -86.692345,
                -97.563461, -111.862434, -72.710686, -78.169968, -121.490494, -80.954570, -89.616508, -107.302490)
)

# Now merge the data based on state names to have a dataframe with coordinates and data
states_data <- left_join(states, merged_df, by = "state")

# Load and prepare state map data from ggplot by excluding Alaska and Hawaii
states_map <- map_data("state") %>%
  filter(!region %in% c("alaska", "hawaii")) %>%
  mutate(state = tolower(region))  # Convert the 'region' to lowercase

# Convert the map data to an sf object and combine geometries
states_sf <- st_as_sf(states_map, coords = c("long", "lat"), crs = 4326) %>%
  group_by(state) %>%
  summarize(geometry = st_combine(geometry), .groups = "drop") %>%
  st_cast("POLYGON")

states_data_sf <- left_join(states_sf, states_data, by = "state")
states_data_sf <- states_data_sf[states_data_sf$state != "district of columbia", ]

state_abbrs <- setNames(state.abb, tolower(state.name))

# Create a new column for state abbreviations
states_data_sf <- states_data_sf %>%
  mutate(state_abbr = state_abbrs[state])

states_data_sf <- subset(states_data_sf, select = -c(x, number_of_households_in_1000, percentage_of_households_with_pets, number_of_pet_households_in_1000, percentage_of_dog_owners, cat_owning_households, mean_number_of_cats, dog_owning_households_1000s, mean_number_of_dogs_per_household, percentage_of_cat_owners))

states_data_sf <- states_data_sf %>%
  relocate(state_abbr, .after = state)

colnames(states_data_sf)[colnames(states_data_sf) == "cat_population"] <- "cat_population_in_1000"

ui_vaccine <- fluidPage(
  tabsetPanel(
    tabPanel("Pet Vaccine Rate", leafletOutput("vaccineMap",height = "600px"))
  )
)

server_vaccine <- function(input, output) {
  states_data_sf <- left_join(states_sf, states_data, by = "state")
  
  # create color palettes excluding NA values
  pal <- function(variable) {
    valid_values <- na.omit(states_data_sf[[variable]])
    colorNumeric(palette = "BuGn", domain = valid_values, na.color = "#FFFFFF")
  }
  
  output$vaccineMap <- renderLeaflet({
    vaccine_pal <- pal("pet_vaccine_rate")
    
    leaflet(states_data_sf) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~vaccine_pal(pet_vaccine_rate),
        weight = 2,
        color = "#444444",  # Dark grey borders
        fillOpacity = 0.7,
        popup = ~paste(toupper(state), "<br>", "Pet Vaccine Rate", pet_vaccine_rate, "%"),
        label = ~state.abb[match(tolower(state), tolower(state.name))],  # Add state abbreviations as labels
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = 'center',
          textOnly = TRUE,
          style = list("font-weight" = "bold", "font-size" = "9px", "color" = "black")
        )
      ) %>%
      addLegend(
        pal = vaccine_pal,
        values = ~pet_vaccine_rate,
        title = "Vaccine Rate (%)",
        opacity = 0.7,
        position = "bottomright",
        labFormat = labelFormat(suffix = "%"),
        na.label = ""  # Exclude NA values from the legend
      ) %>%
      setView(lng = -96, lat = 37.8, zoom = 4)
  })
}

# Create leaftlet bivariate choropleth map to show dog vs cat population
# not shiny app because of the difficulties to modifying legend format catering to shiny 

cat_palette <- colorNumeric(palette = "Blues", domain = range(states_data_sf$cat_population_in_1000, na.rm = TRUE), na.color = NA)
dog_palette <- colorNumeric(palette = "Reds", domain = range(states_data_sf$dog_population_in_1000, na.rm = TRUE), na.color = NA)

# Update the get_bivariate_color function to handle NAs and correctly blend colors
get_bivariate_color <- function(cat_val, dog_val) {
  if (is.na(cat_val) || is.na(dog_val)) {
    return(NA_character_)
  }
  # Get color for cat and dog values using their respective palettes
  cat_color <- cat_palette(cat_val)
  dog_color <- dog_palette(dog_val)
  
  # Blend the colors by averaging the RGB values
  cat_col_rgb <- col2rgb(cat_color) / 255
  dog_col_rgb <- col2rgb(dog_color) / 255
  blended_rgb <- (cat_col_rgb + dog_col_rgb) / 2
  
  # Return the color in hexadecimal format
  return(rgb(blended_rgb[1], blended_rgb[2], blended_rgb[3], max = 1))
}

# Apply the get_bivariate_color function to create a bivariate color for each state
states_data_sf$bivariate_color <- mapply(get_bivariate_color, states_data_sf$cat_population_in_1000, states_data_sf$dog_population_in_1000)

# Define the breaks for low, medium, and high values
cat_breaks <- quantile(states_data_sf$cat_population_in_1000, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
dog_breaks <- quantile(states_data_sf$dog_population_in_1000, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)

# Create a data frame for the legend
legend_data <- data.frame(
  cat = factor(rep(c("Low", "Medium", "High"), each = 3), levels = c("Low", "Medium", "High")),
  dog = factor(rep(c("Low", "Medium", "High"), times = 3), levels = c("Low", "Medium", "High"))
)

# Compute the color for each combination in the legend
legend_data$color <- mapply(function(cat, dog) {
  cat_val <- switch(cat, Low = cat_breaks[2], Medium = cat_breaks[3], High = cat_breaks[4])
  dog_val <- switch(dog, Low = dog_breaks[2], Medium = dog_breaks[3], High = dog_breaks[4])
  get_bivariate_color(cat_val, dog_val)
}, legend_data$cat, legend_data$dog)

# leaflet map
map <- leaflet(states_data_sf) %>%
  addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE)) %>%
  addPolygons(
    fillColor = ~bivariate_color,
    fillOpacity = 0.7,
    color = "white",
    weight = 1,
    popup = ~paste0(
      "<strong>", toupper(state), "</strong><br>",
      "Cat Population: ", round(cat_population_in_1000, 2), "k<br>",
      "Dog Population: ", round(dog_population_in_1000, 2), "k"
    ),
    label = ~toupper(state.abb[match(tolower(state), tolower(state.name))]),
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = 'center',
      textOnly = TRUE,
      style = list("font-weight" = "bold", "font-size" = "9px", "color" = "black")
    )
  ) %>%
  setView(-96, 37.8, 4)

# Add the legend using a custom control
legend <- tags$div(
  tags$style(HTML("
    .legend-title {
      text-align: center;
      color: black;
      font-weight: bold;
      font-size: 12px;
      margin-bottom: 3px;
    }
    .legend-scale {
      width: 200px;
      height: 100px;
      display: grid;
      grid-template-columns: repeat(3, 1fr);
      grid-template-rows: repeat(3, 1fr);
    }
    .legend-label {
      font-size: 8px;
      color: #B22222; 
      text-align: center;
    }
  ")),
  tags$div(class = "legend-title", "Cat-Dog Populations"),
  tags$div(class = "legend-scale",
           lapply(seq_along(legend_data$color), function(i) {
             tags$div(style = paste0("background-color: ", legend_data$color[i], ";"),
                      class = "legend-label",
                      paste(legend_data$cat[i], "-", legend_data$dog[i]))
           })
  )
)

# Add the legend to the map
map <- map %>% addControl(legend, position = "bottomright")

map

states_data_sf <- states_data_sf %>% select(-total_cost_average_dollar)

states_data_sf$total_cost_average_dollar <- with(states_data_sf, food_average_dollar + toys_and_treats_average_dollar + medical_average_dollar + household_items_average_dollar + grooming_average_dollar)

states_data_sf <- states_data_sf %>%
  relocate(total_cost_average_dollar, .after = cat_population_in_1000 )

ui_expen <- fluidPage(
  titlePanel("Navigating Pet Expenses Across the States"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selectedState", "Select State(s):",
                  choices = c("All", unique(states_data_sf$state)),
                  multiple = TRUE,
                  selected = "All"),
      helpText("Hold down 'Ctrl' on Windows or 'Cmd' on Mac to select multiple states, or select 'All' to display all states.")
    ),
    mainPanel(
      plotlyOutput("plot", height = "600px", width = "100%"),
      verbatimTextOutput("statsTotalCost")
    )
  )
)

server_expen <- function(input, output) {
  output$plot <- renderPlotly({
    # Determine if "All" is selected; if so, use all states
    selected_states <- if("All" %in% input$selectedState) {
      unique(states_data_sf$state)
    } else {
      input$selectedState
    }
    
    states_data <- if("sf" %in% class(states_data_sf)) {
      as.data.frame(states_data_sf)
    } else {
      states_data_sf
    }
    
    # Filter data based on selected states
    filtered_data <- states_data %>%
      filter(state %in% selected_states) %>%
      select(state, total_cost_average_dollar, food_average_dollar,
             toys_and_treats_average_dollar, medical_average_dollar,
             household_items_average_dollar, grooming_average_dollar)
    
    long_data <- filtered_data %>%
      pivot_longer(cols = c(food_average_dollar, toys_and_treats_average_dollar,
                            medical_average_dollar, household_items_average_dollar,
                            grooming_average_dollar),
                   names_to = "category", values_to = "cost")
    
    long_data <- long_data %>%
      group_by(state) %>%
      mutate(percent = (cost / total_cost_average_dollar) * 100) %>%
      ungroup()
    
    p <- plot_ly(data = long_data, x = ~state, y = ~cost, type = 'bar', color = ~category,
                 text = ~paste(state, "<br>",category, ": $", round(cost, 2), "<br>", "Percent of Total: ", round(percent, 2), "%"),
                 hoverinfo = 'text', showlegend = TRUE) %>%
      layout(barmode = 'stack',
             title = "Average Annual Pet Costs by State - 2022",
             xaxis = list(title = "State"),
             yaxis = list(title = "Cost ($)"),
             margin = list(b = 150))
    
    p
  })
  
  # Add a new output for the descriptive stats
  output$statsTotalCost <- renderPrint({
    selected_states <- if("All" %in% input$selectedState) {
      unique(states_data_sf$state)
    } else {
      input$selectedState
    }
    
    # Filter the data
    filtered_data <- states_data_sf %>%
      filter(state %in% selected_states)
    
    # If "All" is selected or multiple states are selected, provide combined stats
    if(length(selected_states) > 1) {
      # Calculate summary stats for all selected states combined
      stats <- summary(filtered_data$total_cost_average_dollar)
      print(stats)
    } else if(length(selected_states) == 1) {
      # Calculate summary stats for the single selected state
      stats <- summary(filtered_data$total_cost_average_dollar[filtered_data$state == selected_states])
      print(stats)
    } else {
      return("No state selected.")
    }
  })
}

states_data_sf <- states_data_sf %>%
  mutate(adoption_return_rate = total_live_outcome_returned_to_owner / total_live_outcome_adoption) %>%
  relocate(adoption_return_rate, .before = bivariate_color)

ui_return <- fluidPage(
  
  tabsetPanel(
    tabPanel("Pet return rate after adoption", leafletOutput("adoptionRateMap", height = "600px")),
  )
)

server_return <- function(input, output) {
  states_data_sf <- left_join(states_sf, states_data, by = "state") %>%
    mutate(adoption_return_rate = total_live_outcome_returned_to_owner / total_live_outcome_adoption) # Calculate the adoption return rate
  
  # Filter out rows where adoption_return_rate is NA
  filtered_data <- states_data_sf %>%
    filter(!is.na(adoption_return_rate))
  
  # Create a map for the 'adoption rate' tab
  output$adoptionRateMap <- renderLeaflet({
    # Create color palette based on adoption_return_rate values of the filtered data
    adoption_pal <- colorNumeric(palette = "viridis", domain = filtered_data$adoption_return_rate)
    
    leaflet(filtered_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~adoption_pal(adoption_return_rate),
        weight = 2,
        color = "#444444",
        fillOpacity = 0.7,
        popup = ~paste(toupper(state), "<br>", "Adoption Return Rate", sprintf("%.2f%%", 100 * adoption_return_rate)),
        label = ~state.abb[match(tolower(state), tolower(state.name))],
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = 'center',
          textOnly = TRUE,
          style = list("font-weight" = "bold", "font-size" = "9px", "color" = "black")
        )
      ) %>%
      addLegend(
        pal = adoption_pal,
        values = ~adoption_return_rate,
        title = "Pet Adoption Return Rate (%)",
        opacity = 0.7,
        labFormat = labelFormat(transform = function(x) 100 * x, suffix = "%"), # Transform the legend values to percentages
        position = "bottomright"
      ) %>%
      setView(lng = -96, lat = 37.8, zoom = 4)
  })
}

ui_chip <- fluidPage(
  tabsetPanel(
    tabPanel("Microchip Rate", leafletOutput("microchipRateMap", height = "600px"))
    # Include other tabs as needed
  )
)

server_chip <- function(input, output) {
  
  states_data_sf <- left_join(states_sf, states_data, by = "state")
  
  # Remove rows with NA in 'pet_microchip_rate' before creating the palette
  states_data_sf <- states_data_sf %>%
    filter(!is.na(pet_microchip_rate))
  
  output$microchipRateMap <- renderLeaflet({
    # Create color palette based on pet_microchip_rate values without NA values
    microchip_pal <- colorNumeric(palette = "PuRd", domain = states_data_sf$pet_microchip_rate)
    
    leaflet(states_data_sf) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~microchip_pal(pet_microchip_rate),
        weight = 2,
        color = "#444444",
        fillOpacity = 0.7,
        popup = ~paste(toupper(state), "<br>", "Pet Microchip Rate:", sprintf("%d%%", pet_microchip_rate)),
        label = ~state.abb[match(tolower(state), tolower(state.name))],
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = 'center',
          textOnly = TRUE,
          style = list("font-weight" = "bold", "font-size" = "9px", "color" = "black")
        )
      ) %>%
      addLegend(
        pal = microchip_pal,
        values = ~pet_microchip_rate,
        title = "Pet Microchip Rate (%)",
        opacity = 0.7,
        labFormat = labelFormat(suffix = "%"), # Add percentage symbol to legend labels
        position = "bottomright"
      ) %>%
      setView(lng = -96, lat = 37.8, zoom = 4)
  })
}


# Calculate national averages from the data
national_avg_vaccine_rate <- mean(states_data_sf$pet_vaccine_rate, na.rm = TRUE)
national_avg_microchip_rate <- mean(states_data_sf$pet_microchip_rate, na.rm = TRUE)
national_avg_return_rate <- mean(states_data_sf$adoption_return_rate, na.rm = TRUE)
national_avg_expenditure <- mean(states_data_sf$total_cost_average_dollar, na.rm = TRUE)

# Add the score columns to the dataframe
states_data_sf <- states_data_sf %>%
  mutate(
    vaccine_score = (pet_vaccine_rate / national_avg_vaccine_rate) * 100,
    microchip_score = (pet_microchip_rate / national_avg_microchip_rate) * 100,
    return_rate_score = (1 - (adoption_return_rate / national_avg_return_rate)) * 100,
    expenditure_score = (national_avg_expenditure / total_cost_average_dollar) * 100
  )

# Weights for the final score
weights <- c(vaccine = 0.30, microchip = 0.25, return_rate = 0.20, expenditure = 0.25)

# Calculate the final score
states_data_sf <- states_data_sf %>%
  rowwise() %>%
  mutate(
    final_score = vaccine_score * weights['vaccine'] +
      microchip_score * weights['microchip'] +
      return_rate_score * weights['return_rate'] +
      expenditure_score * weights['expenditure']
  ) %>%
  ungroup()

ui_score <- fluidPage(
  titlePanel("State-level pet owner care performance metrics"),
  tabsetPanel(
    tabPanel("Pet Care Score",
             leafletOutput("finalScoreMap", height = "600px"),
             
             HTML("<p>The Pet Care Score is a composite index that reflects the overall state of pet welfare and owner commitment across different states. 
           This score is calculated using four key components:</p>
           <ul>
             <li><strong>Vaccination Rate Score:</strong> Represents how the state's pet vaccination rate compares to the national average. A higher rate yields a higher score.</li>
             <li><strong>Microchip Rate Score:</strong> Assesses the state's pet microchipping rate relative to the national average, with higher rates contributing positively to the score.</li>
             <li><strong>Return Rate Score:</strong> Measures the inverse of the pet adoption return rate against the national average. States with lower return rates score higher, indicating more successful adoptions.</li>
             <li><strong>Expenditure Score:</strong> Compares the average pet expenditure in the state to the national average. States where the expenditure is lower than the average receive a higher score, suggesting more efficient pet care spending.</li>
           </ul>
           <p>Each component is weighted differently, reflecting its relative importance. The final score is the sum of these weighted scores, where:</p>
           <ul>
             <li>Vaccination Rate contributes 30%</li>
             <li>Microchip Rate contributes 25%</li>
             <li>Return Rate contributes 20%</li>
             <li>Expenditure contributes 25%</li>
           </ul>
           <p>The final Pet Care Score enables a comparative analysis of pet care standards across states.</p>"),
             
    )
  )
)

server_score <- function(input, output) {
  output$finalScoreMap <- renderLeaflet({
    
    final_score_range <- range(states_data_sf$final_score, na.rm = TRUE)
    
    # Create a color palette starting near yellow, not including red
    final_score_pal <- colorNumeric(palette = rainbow(10, start = 1/8, end = 7/8), domain = final_score_range)
    
    
    leaflet(states_data_sf) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~final_score_pal(final_score),
        weight = 2,
        color = "#444444",
        fillOpacity = 0.7,
        popup = ~paste(toupper(state), "<br>", "Final Score:", sprintf("%.2f", final_score)),
        label = ~state.abb[match(tolower(state), tolower(state.name))],
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = 'center',
          textOnly = TRUE,
          style = list("font-weight" = "bold", "font-size" = "9px", "color" = "black")
        )
      ) %>%
      addLegend(
        pal = final_score_pal,
        values = ~final_score,
        title = "Final Score",
        opacity = 0.7,
        position = "bottomright"
      ) %>%
      setView(lng = -96, lat = 37.8, zoom = 4)
  })
}

ui_main <- dashboardPage(
  dashboardHeader(titleWidth = 290, title = "2022 State-Level Pet Data Insights"),
  dashboardSidebar(
    width = 290,
    sidebarMenu(
      menuItem("Overall Pet Care Score", tabName = "health_scores", icon = icon("heartbeat")),
      menuItem("Vaccination Rate", tabName = "vaccination", icon = icon("syringe")),
      menuItem("Microchip Rate", tabName = "microchip", icon = icon("microchip")),
      menuItem("Rate of Adopted Pets Returned to Shelters", tabName = "return_rates", icon = icon("reply")),
      menuItem("Average Pet Spending by Households", tabName = "spending", icon = icon("wallet"))
    )
  ),
  dashboardBody(
    # Main content areas of the app
    tabItems(
      tabItem(tabName = "vaccination", ui_vaccine),
      tabItem(tabName = "spending", ui_expen),
      tabItem(tabName = "return_rates", ui_return),
      tabItem(tabName = "microchip", ui_chip),
      tabItem(tabName = "health_scores", ui_score)
    )
  )
)

server_main <- function(input, output) {
  server_score(input, output)
  server_vaccine(input, output)
  server_chip(input, output)
  
  server_return(input, output)
  server_expen(input, output)
}

# Run the app
shinyApp(ui_main, server_main)

