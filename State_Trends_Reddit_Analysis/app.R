#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

data <- read.csv("output.csv")
head(data)
options(repos = c(CRAN = "https://cran.rstudio.com/"))

if (!require("topicmodels")) install.packages("topicmodels", dependencies = TRUE)
library(topicmodels)
library(tm)
if (!require("slam")) install.packages("slam")
library(slam)
if (!require(ldatuning)) install.packages("ldatuning", dependencies = TRUE)
library(ldatuning)
library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)
library(plotly) # 添加 plotly 库
library(shinydashboard)
library(shinyjs)
library(wordcloud2)
library(RColorBrewer)
library(reshape2)


stopwords <- scan("stopwords-en.txt", what = "character", sep = "\n")
stopwords_names <- scan("StopWords_Names.txt", what = "character", sep = "\n")

corpus <- Corpus(VectorSource(data$Post_text))


corpus_clean <- tm_map(corpus, content_transformer(tolower))  
corpus_clean <- tm_map(corpus_clean, removePunctuation)  
corpus_clean <- tm_map(corpus_clean, removeNumbers)  
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords)  
corpus_clean <- tm_map(corpus_clean, stripWhitespace)  


corpus_clean <- tm_map(corpus_clean, content_transformer(function(x) gsub("[^a-zA-Z\\s]", " ", x)))


remove_words_fixed <- function(text, stopwords) {
  for (word in stopwords) {
    text <- gsub(paste0("\\b", word, "\\b"), "", text, fixed = TRUE)
  }
  return(text)
}


corpus_clean_second <- tm_map(corpus_clean, content_transformer(remove_words_fixed), stopwords_names)


corpus_clean_second <- tm_map(corpus_clean_second, content_transformer(function(x) gsub("[^a-zA-Z\\s]", " ", x)))
corpus_clean_second <- tm_map(corpus_clean_second, stripWhitespace)  


inspect(corpus_clean_second[1:5])  



files <- c("Ethical.txt", "Dog.txt", "Cat.txt", "tips.txt")
text_data <- lapply(files, function(f) readLines(f, warn = FALSE))  
text_data <- unlist(text_data)  
corpus <- Corpus(VectorSource(text_data))  

# 数据清洗过程
corpus_clean1 <- tm_map(corpus, content_transformer(tolower))  
corpus_clean1 <- tm_map(corpus_clean1, removePunctuation)  
corpus_clean1 <- tm_map(corpus_clean1, removeNumbers)  
corpus_clean1 <- tm_map(corpus_clean1, removeWords, stopwords)  
corpus_clean1 <- tm_map(corpus_clean1, stripWhitespace)  
corpus_clean1 <- tm_map(corpus_clean1, content_transformer(function(x) gsub("[^a-zA-Z\\s]", " ", x)))  


remove_words_fixed <- function(text, stopwords) {
  for (word in stopwords) {
    text <- gsub(paste0("\\b", word, "\\b"), "", text, fixed = TRUE)
  }
  return(text)
}
corpus_clean <- tm_map(corpus_clean1, content_transformer(remove_words_fixed), stopwords_names)


corpus_clean1 <- tm_map(corpus_clean1, content_transformer(function(x) gsub("[^a-zA-Z\\s]", " ", x)))
corpus_clean1 <- tm_map(corpus_clean1, stripWhitespace)  


inspect(corpus_clean1[1:5])  


dtm1 <- DocumentTermMatrix(corpus_clean1)


word_freqs <- as.integer(col_sums(dtm1))
word_freqs <- sort(word_freqs, decreasing = TRUE)  


terms <- Terms(dtm1)  


word_freqs_named <- data.frame(term = terms, freq = word_freqs)
word_freqs_named <- word_freqs_named[order(-word_freqs_named$freq), ]  


top_words <- head(word_freqs_named, 100)


print(top_words)

words_only <- top_words$term


nonpets_stop_words <- c("ethical", "breeder", "bills", "vet", "breed", "welfare",
                        "dna", "neogen")


pets_stop_words <- top_words$term[!top_words$term %in% nonpets_stop_words]

print(pets_stop_words)

additional_words <- c("dog", "animal", "people", "animals", "pets")
pets_stop_words <- c(pets_stop_words, additional_words)

print(pets_stop_words)

corpus_clean_final <- tm_map(corpus_clean_second, content_transformer(function(x) remove_words_fixed(x, pets_stop_words)))

corpus_clean_final <- tm_map(corpus_clean_final, content_transformer(function(x) gsub("[^a-zA-Z\\s]", " ", x)))
corpus_clean_final <- tm_map(corpus_clean_final, stripWhitespace)

remove_specific_words <- function(text, words_to_remove) {
  
  for (word in words_to_remove) {
    text <- gsub(paste0("\\b", word, "\\b"), "", text, fixed = FALSE)
  }
  
  text <- gsub("\\s+", " ", text)
  return(trimws(text))
}


specific_words <- c("dog", "cat", "pet", "pets", "cats", "dogs","male","female","animal","animals","time","house", "day", "puppy" ,"peopole","kitten")


corpus_clean_final <- tm_map(corpus_clean_final, content_transformer(function(x) remove_specific_words(x, specific_words)))


inspect(corpus_clean_final[1:5])  



stopwords_additional <- scan("stopwords-en.txt", what = "character", sep = "\n")

remove_additional_stopwords <- function(text, stopwords) {
  for (word in stopwords) {
    text <- gsub(paste0("\\b", word, "\\b"), "", text, fixed = TRUE)
  }
  text <- gsub("\\s+", " ", text)  
  return(trimws(text))  
}


corpus_clean_final <- tm_map(corpus_clean_final, content_transformer(function(x) remove_additional_stopwords(x, stopwords_additional)))


inspect(corpus_clean_final[1:5])  



dtm2 <- DocumentTermMatrix(corpus_clean_final)

num_topics <- 5  
lda_model <- LDA(dtm2, k = num_topics, control = list(seed = 1234))

topics <- terms(lda_model, 15)
print(topics)

topic_terms <- terms(lda_model, 15)

topics_frequent_words <- list()

for (topic in 1:num_topics) {
  words <- topic_terms[, topic]
  
  
  valid_words <- intersect(colnames(as.matrix(dtm2)), words)
  
  
  word_freq <- colSums(as.matrix(dtm2)[, valid_words, drop=FALSE])
  
  
  topics_frequent_words[[paste("Topic", topic)]] <- sort(word_freq, decreasing = TRUE)
}

print(topics_frequent_words)

topic_coherence_scores <- numeric(num_topics)

for (topic in 1:num_topics) {
  topic_words <- topic_terms[, topic]
  
  valid_words <- intersect(colnames(as.matrix(dtm2)), topic_words)
  
  if (length(valid_words) >= 2) {
    
    co_occurrence_matrix <- crossprod(as.matrix(dtm2[, valid_words, drop=FALSE]))
    
    upper_tri <- co_occurrence_matrix[upper.tri(co_occurrence_matrix)]
    
    topic_coherence_scores[topic] <- mean(upper_tri)
  } else {
    topic_coherence_scores[topic] <- NA  
  }
}


print(topic_coherence_scores)


all_topics_data <- data.frame(Topic = factor(), Word = character(), Frequency = integer(), stringsAsFactors = FALSE)
topic_names <- c("1. Family and Life Advice", "2. Pet Care and Daily Management",
                 "3. Pet Health and Medical Emergencies", "4. Emotional Care and Nighttime Anxiety",
                 "5. Pet Diet and Weight Management")

for (topic in 1:num_topics) {
  topic_data <- topics_frequent_words[[paste("Topic", topic)]]
  df <- data.frame(
    Topic = factor(topic_names[topic]),
    Word = names(topic_data),
    Frequency = topic_data,
    stringsAsFactors = FALSE
  )
  all_topics_data <- rbind(all_topics_data, df)
}



num_words = 15


topics_df <- data.frame(matrix(ncol = num_words + 1, nrow = num_topics))
colnames(topics_df) <- c(paste("Word", 1:num_words, sep = ""), "CoherenceScore")


for (topic in 1:num_topics) {
  words <- topic_terms[, topic]
  valid_words <- intersect(colnames(as.matrix(dtm2)), words)
  
  
  word_freqs <- colSums(as.matrix(dtm2)[, valid_words, drop=FALSE])
  
  
  top_words <- sort(word_freqs, decreasing = TRUE)
  if (length(top_words) > num_words) {
    top_words <- names(top_words)[1:num_words]
  } else {
    top_words <- c(names(top_words), rep(NA, num_words - length(top_words)))
  }
  
  
  topics_df[topic, 1:num_words] <- top_words
  topics_df[topic, "CoherenceScore"] <- topic_coherence_scores[topic]
}


rownames(topics_df) <- paste("Topic", 1:num_topics, sep = "")


print(topics_df)


all_topics_df <- data.frame(Word=character(), Frequency=integer(), stringsAsFactors=FALSE)


for (topic in names(topics_frequent_words)) {
  topic_data <- topics_frequent_words[[topic]]
  
  df <- data.frame(Word = names(topic_data), Frequency = topic_data, stringsAsFactors = FALSE)
  
  all_topics_df <- rbind(all_topics_df, df)
}


final_df <- all_topics_df %>%
  group_by(Word) %>%
  summarise(Frequency = max(Frequency)) %>%
  arrange(desc(Frequency))


print(final_df)


selected_words <- c("vet", "food", "feel", "advice", "litter", "owner", "eat", "surgery",
                    "shelter", "pain", "box", "cage", "blood", "fleas", "emergency", "insurance")


new_final_df <- final_df[final_df$Word %in% selected_words, ]


print(new_final_df)

file_names <- c("Ethical.txt", "Dog.txt", "Cat.txt")
stop_words <- scan("stopwords-en.txt", what = "character", sep = "\n")
stop_words_names <- scan("StopWords_Names.txt", what = "character", sep = "\n")

raw_text_data <- lapply(file_names, function(f) readLines(f, warn = FALSE))
raw_text_data <- unlist(raw_text_data)

text_corpus <- Corpus(VectorSource(raw_text_data))

text_corpus <- tm_map(text_corpus, content_transformer(tolower))

text_corpus <- tm_map(text_corpus, removePunctuation)

text_corpus <- tm_map(text_corpus, removeNumbers)

text_corpus <- tm_map(text_corpus, removeWords, stop_words)

text_corpus <- tm_map(text_corpus, stripWhitespace)

text_corpus <- tm_map(text_corpus, content_transformer(function(x) gsub("[^a-zA-Z\\s]", " ", x)))


remove_fixed_words <- function(text, stop_words) {
  for (word in stop_words) {
    text <- gsub(paste0("\\b", word, "\\b"), "", text, fixed = TRUE)
  }
  return(text)
}


text_corpus <- tm_map(text_corpus, content_transformer(remove_fixed_words), stop_words_names)

text_corpus <- tm_map(text_corpus, content_transformer(function(x) gsub("[^a-zA-Z\\s]", " ", x)))

text_corpus <- tm_map(text_corpus, stripWhitespace)


writeLines(sapply(text_corpus, as.character), "cleaned_texts.txt")




clean_text_corpus <- function(text_corpus) {
  text_corpus <- tm_map(text_corpus, content_transformer(tolower))
  text_corpus <- tm_map(text_corpus, removePunctuation)
  text_corpus <- tm_map(text_corpus, removeNumbers)
  text_corpus <- tm_map(text_corpus, removeWords, stop_words)
  text_corpus <- tm_map(text_corpus, content_transformer(function(x) gsub("[^a-zA-Z\\s]", " ", x)))
  text_corpus <- tm_map(text_corpus, stripWhitespace)
  return(text_corpus)
}

train_topic_model <- function(text_corpus) {
  dtm <- DocumentTermMatrix(text_corpus)
  lda_result <- LDA(dtm, k = 5, control = list(seed = 1234))
  topic_words <- terms(lda_result, 15)
  return(topic_words)
}


seasons <- unique(data$Season)
seasonal_topic_trends <- list()
for (season in seasons) {
  subset_data <- data[data$Season == season,]
  text_corpus <- Corpus(VectorSource(subset_data$Post_text))
  cleaned_text_corpus <- clean_text_corpus(text_corpus)
  topic_words <- train_topic_model(cleaned_text_corpus)
  seasonal_topic_trends[[season]] <- topic_words
}


print(seasonal_topic_trends)


seasonal_topic_data <- data.frame(Season = character(), Topic = character(), Word = character(), stringsAsFactors = FALSE)


for (season in names(seasonal_topic_trends)) {
  
  season_data <- seasonal_topic_trends[[season]]
  if (is.matrix(season_data)) {
    num_topics <- ncol(season_data)
    if (num_topics > 0) {
      
      for (topic_idx in 1:num_topics) {
        
        df <- data.frame(
          Season = season,
          Topic = paste("Topic", topic_idx),
          Word = season_data[, topic_idx],
          stringsAsFactors = FALSE
        )
        
        seasonal_topic_data <- rbind(seasonal_topic_data, df)
      }
    } else {
      warning(paste("No topics available for season:", season))
    }
  } else {
    warning(paste("Data for season", season, "is not a matrix."))
  }
}


print(seasonal_topic_data)



seasonal_topic_data <- seasonal_topic_data %>%
  count(Season, Word) %>%
  pivot_wider(names_from = Word, values_from = n, values_fill = list(n = 0)) # 更正为 pivot_wider


############--------------------------------------------------------------------
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
           This score is calculated using four key components:</p >
           <ul>
             <li><strong>Vaccination Rate Score:</strong> Represents how the state's pet vaccination rate compares to the national average. A higher rate yields a higher score.</li>
             <li><strong>Microchip Rate Score:</strong> Assesses the state's pet microchipping rate relative to the national average, with higher rates contributing positively to the score.</li>
             <li><strong>Return Rate Score:</strong> Measures the inverse of the pet adoption return rate against the national average. States with lower return rates score higher, indicating more successful adoptions.</li>
             <li><strong>Expenditure Score:</strong> Compares the average pet expenditure in the state to the national average. States where the expenditure is lower than the average receive a higher score, suggesting more efficient pet care spending.</li>
           </ul>
           <p>Each component is weighted differently, reflecting its relative importance. The final score is the sum of these weighted scores, where:</p >
           <ul>
             <li>Vaccination Rate contributes 30%</li>
             <li>Microchip Rate contributes 25%</li>
             <li>Return Rate contributes 20%</li>
             <li>Expenditure contributes 25%</li>
           </ul>
           <p>The final Pet Care Score enables a comparative analysis of pet care standards across states.</p >"),
             
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








library(shiny)
library(shinydashboard)
library(leaflet)
library(shinyjs)
library(plotly)
library(ggplot2)
library(wordcloud2)
library(RColorBrewer)
library(reshape2)


ui_merged <- dashboardPage(
  dashboardHeader(
    titleWidth = 290,
    title = tagList(
      tags$a(href = "", target = "_blank", "HTML Link"),
      "Integrated Shiny Modules"
    )
  ),
  dashboardSidebar(
    width = 290,
    sidebarMenu(
      menuItem("2022 State-Level Pet Data", tabName = "pet_data", icon = icon("paw"),
               menuSubItem("Overall Pet Care Score", tabName = "health_scores"),
               menuSubItem("Vaccination Rate", tabName = "vaccination"),
               menuSubItem("Microchip Rate", tabName = "microchip"),
               menuSubItem("Rate of Adopted Pets Returned to Shelters", tabName = "return_rates"),
               menuSubItem("Average Pet Spending by Households", tabName = "spending")
      ),
      menuItem("Reddit Pet Topic NLP Analysis", tabName = "reddit_analysis", icon = icon("reddit"),
               menuSubItem("Topic Analysis", tabName = "topic_analysis_sub"),
               menuSubItem("Coherence Score", tabName = "coherence_score"),
               menuSubItem("Word Frequency Analysis", tabName = "word_frequency"),
               menuSubItem("Season Trend Analysis", tabName = "season_trend_analysis")
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      # Pet Data tabs
      tabItem(tabName = "vaccination", ui_vaccine),
      tabItem(tabName = "spending", ui_expen),
      tabItem(tabName = "return_rates", ui_return),
      tabItem(tabName = "microchip", ui_chip),
      tabItem(tabName = "health_scores", ui_score),
      # Reddit Analysis tabs
      tabItem(tabName = "topic_analysis_sub",
              fluidRow(
                box(
                  selectInput("selected_topic", "Select a topic:", choices = c("1. Family and Life Advice", "2. Pet Care and Daily Management", "3. Pet Health and Medical Emergencies", "4. Emotional Care and Nighttime Anxiety", "5. Pet Diet and Weight Management")),
                  width = 12
                )
              ),
              fluidRow(
                box(
                  plotlyOutput("topic_plot", height = "600px"),
                  div(id = "rationale_text", style = "margin-top: 20px;"),
                  div(style = "margin-top: 20px; text-align: justify;",
                      "This word frequency visualization is based on comments from the pet section between April 2023 and April 2024, using data cleansing and topic analysis to get each topic and its associated words, reflecting the main discussion within the community."),
                  width = 12
                )
              )
      ),
      tabItem(tabName = "coherence_score",
              fluidRow(
                box(
                  plotlyOutput("coherence_plot", height = "600px"),
                  width = 12
                )
              ),
              fluidRow(
                box(
                  h3("About Coherence Scores"),
                  p("Calculating coherence scores is a crucial step in assessing the quality of LDA (Latent Dirichlet Allocation) topic models. This process helps quantify and assess the quality of topic models, particularly in terms of the statistical significance of word co-occurrences."),
                  width = 12,
                  style = "font-size: 80%;"
                )
              )
      ),
      tabItem(tabName = "word_frequency",
              fluidRow(
                box(
                  wordcloud2Output("wordcloud", width = "100%", height = "600px"),
                  width = 12
                )
              )
      ),
      tabItem(tabName = "season_trend_analysis",
              fluidRow(
                box(
                  plotlyOutput("heatmapPlot", height = "600px"),
                  width = 12
                )
              ),
              fluidRow(
                box(
                  p("The words that appear in every season are primarily common types of pets (such as cats, dogs), places or activities related to pets (such as veterinarians, houses, food), and common descriptive words (such as time, love, care). This indicates that these are the most common and fundamental vocabulary in pet-related topics.",
                    "Words that appear only in specific seasons may reflect particular events, topics, or trends of that quarter. For example:",
                    tags$ul(
                      tags$li("The words such as 'disease,' 'effectively,' 'Sweden' that appear in 24s2 may be related to specific pet diseases or medical topics."),
                      tags$li("The words like 'animals,' 'shelter,' and 'surgery' that appear in 24s1 may be associated with topics such as animal shelters, adoption, and medical procedures."),
                      tags$li("The words 'friend,' 'pain,' and 'update' that appear in 23s4 may be related to topics such as pet illness, injury, and updates on their condition."),
                      tags$li("The words 'advice,' 'comments,' 'emergency,' and 'life' that appear in 23s3 may be related to topics such as pet emergencies, seeking advice, and other life-related discussions."),
                      tags$li("The words 'health,' 'kitten,' 'training,' and 'turtle' that appear in 23s2 may be related to the health and training of specific pets such as kittens and turtles.")
                    ),
                    "Seasonal changes may also affect the distribution of topic words in different quarters. For example, in certain seasons, there may be more words related to weather, holidays, and seasonal diseases."
                  ),
                  width = 12
                )
              )
      )
    )
  )
)

library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(plotly)
library(wordcloud2)
library(reshape2)
library(RColorBrewer)

# UI for integrated Shiny modules
ui <- dashboardPage(
  dashboardHeader(titleWidth = 380, title = "Pet Data Insights: State Trends & Reddit NLP Dive"),
  dashboardSidebar(
    width = 380,
    sidebarMenu(
      menuItem("Overall Pet Care Score", tabName = "health_scores", icon = icon("heartbeat")),
      menuItem("Vaccination Rate", tabName = "vaccination", icon = icon("syringe")),
      menuItem("Microchip Rate", tabName = "microchip", icon = icon("microchip")),
      menuItem("Rate of Adopted Pets Returned to Shelters", tabName = "return_rates", icon = icon("reply")),
      menuItem("Average Pet Spending by Households", tabName = "spending", icon = icon("wallet")),
      menuItem("Topic Analysis", tabName = "topic_analysis", icon = icon("comment-alt"),
               menuSubItem("Topic Analysis", tabName = "topic_analysis_sub"),
               menuSubItem("Coherence Score", tabName = "coherence_score"),
               menuSubItem("Word Frequency Analysis", tabName = "word_frequency")
      ),
      menuItem("Season Trend Analysis", tabName = "season_trend_analysis", icon = icon("calendar-alt"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "vaccination", ui_vaccine),
      tabItem(tabName = "spending", ui_expen),
      tabItem(tabName = "return_rates", ui_return),
      tabItem(tabName = "microchip", ui_chip),
      tabItem(tabName = "health_scores", ui_score),
      tabItem(tabName = "topic_analysis_sub",
              fluidRow(
                box(
                  selectInput("selected_topic", "Select a topic:", choices = c("1. Family and Life Advice", "2. Pet Care and Daily Management", "3. Pet Health and Medical Emergencies", "4. Emotional Care and Nighttime Anxiety", "5. Pet Diet and Weight Management")),
                  width = 12
                )
              ),
              fluidRow(
                box(
                  plotlyOutput("topic_plot", height = "600px"),
                  div(id = "rationale_text", style = "margin-top: 20px;"),
                  div(style = "margin-top: 20px; text-align: justify;",
                      "This word frequency visualization is based on comments from the pet section between April 2023 and April 2024, using data cleansing and topic analysis to get each topic and its associated words, reflecting the main discussion within the community."),
                  width = 12
                )
              )
      ),
      tabItem(tabName = "coherence_score",
              fluidRow(
                box(
                  plotlyOutput("coherence_plot", height = "600px"),
                  width = 12
                )
              ),
              fluidRow(
                box(
                  h3("About Coherence Scores"),
                  p("Calculating coherence scores is a crucial step in assessing the quality of LDA (Latent Dirichlet Allocation) topic models. This process helps quantify and assess the quality of topic models, particularly in terms of the statistical significance of word co-occurrences."),
                  width = 12,
                  style = "font-size: 80%;"
                )
              )
      ),
      tabItem(tabName = "word_frequency",
              fluidRow(
                box(
                  wordcloud2Output("wordcloud", width = "100%", height = "600px"),
                  width = 12
                )
              )
      ),
      tabItem(tabName = "season_trend_analysis",
              fluidRow(
                box(
                  plotlyOutput("heatmapPlot", height = "600px"),
                  width = 12
                )
              ),
              fluidRow(
                box(
                  p("The words that appear in every season are primarily common types of pets (such as cats, dogs), places or activities related to pets (such as veterinarians, houses, food), and common descriptive words (such as time, love, care). This indicates that these are the most common and fundamental vocabulary in pet-related topics.",
                    "Words that appear only in specific seasons may reflect particular events, topics, or trends of that quarter. For example:",
                    tags$ul(
                      tags$li("The words such as 'disease,' 'effectively,' 'Sweden' that appear in 24s2 may be related to specific pet diseases or medical topics."),
                      tags$li("The words like 'animals,' 'shelter,' and 'surgery' that appear in 24s1 may be associated with topics such as animal shelters, adoption, and medical procedures."),
                      tags$li("The words 'friend,' 'pain,' and 'update' that appear in 23s4 may be related to topics such as pet illness, injury, and updates on their condition."),
                      tags$li("The words 'advice,' 'comments,' 'emergency,' and 'life' that appear in 23s3 may be related to topics such as pet emergencies, seeking advice, and other life-related discussions."),
                      tags$li("The words 'health,' 'kitten,' 'training,' and 'turtle' that appear in 23s2 may be related to the health and training of specific pets such as kittens and turtles.")
                    ),
                    "Seasonal changes may also affect the distribution of topic words in different quarters. For example, in certain seasons, there may be more words related to weather, holidays, and seasonal diseases."
                  ),
                  width = 12
                )
              )
      )
    )
  )
)

# Server function for integrated Shiny modules
server <- function(input, output) {
  server_score(input, output)
  server_vaccine(input, output)
  server_chip(input, output)
  
  server_return(input, output)
  server_expen(input, output)
  
  # Additional server logic for module 2
  topic_rationale <- function() {
    list(
      "1. Family and Life Advice" = "The frequent use of words such as 'feel', 'care', 'love', and 'family' suggests that this topic is centered around emotional support and familial advice. It highlights a discourse rich in affectivity and concern for the well-being of family members, suggesting that users are seeking or offering guidance on nurturing family bonds, understanding emotional needs, and creating a supportive home environment. This pattern of word frequency reflects the community's collective emphasis on the importance of emotional care within the family dynamic.",
      "2. Pet Care and Daily Management" = "The recurring appearance of words like 'food', 'morning', and 'night' indicates a focus on daily care and pet management. These terms point to a routine-centric discussion within the community, centered on the regular aspects of pet care, including feeding schedules, morning activities, and nighttime routines. The prominence of such words in the discourse reflects a shared interest in maintaining the health and well-being of pets through consistent daily practices.",
      "3. Pet Health and Medical Emergencies" = "The prevalence of health-related terms such as 'vet', 'sick', and 'emergency' underscores the medical aspect of pet care. These keywords signal a community's engagement with topics concerning veterinary visits, illness management, and urgent care for pets, highlighting the importance placed on the health and medical needs of animals. The focus on such terms reflects a collective concern for recognizing and responding to health issues, ensuring timely professional care, and preparing for potential emergencies in pet ownership.",
      "4. Emotional Care and Nighttime Anxiety" = "The presence of words such as 'love', 'night', and 'anxiety' points to discussions around emotional support and stress management for pets. These terms suggest a compassionate approach to addressing pets' emotional well-being, dealing with nighttime restlessness, and alleviating anxiety. This pattern indicates the community's awareness of the psychological needs of pets and the desire to foster an environment that nurtures their mental health. It underscores the importance of emotional bonding and providing a calming presence for pets facing stress or discomfort.",
      "5. Pet Diet and Weight Management" = "The frequent mentions of 'food', 'diet', 'weight', and 'feeding' are associated with the topics of pet nutrition and weight management. These terms reflect a significant concern and active discussion within the community regarding the proper dietary habits, the importance of balanced food intake, and strategies for maintaining a healthy weight for pets. This focus underscores the commitment to ensuring pets receive the appropriate nutrients for their health and vitality while preventing obesity and related health issues through mindful feeding practices."
    )
  }
  
  reactive_data <- reactive({
    req(input$selected_topic)
    df <- all_topics_data %>% filter(Topic == input$selected_topic)
    df$WordReorder <- reorder(df$Word, -df$Frequency)
    return(df)
  })
  
  output$topic_plot <- renderPlotly({
    req(input$selected_topic)
    df <- reactive_data()
    topic_specific_color <- topic_colors[df$Topic[1]]
    p <- ggplot(df, aes(x = WordReorder, y = Frequency, fill = Topic, text = paste("Word:", Word, "<br>Frequency:", Frequency))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = topic_specific_color) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(size = 12, face = "bold"),
            plot.title = element_text(size = 20, face = "bold"),
            axis.title.y = element_text(size = 16, face = "bold")) +
      labs(x = "Word", y = "Frequency", title = input$selected_topic, fill = "Topic")
    ggplotly(p, tooltip = "text")
  })
  
  observeEvent(input$selected_topic, {
    req(input$selected_topic)
    rationale <- topic_rationale()[input$selected_topic]
    shinyjs::html("rationale_text", sprintf("<strong>Reason for topic naming:</strong> %s", rationale))
  })
  
  output$coherence_plot <- renderPlotly({
    req(input$selected_topic)
    
    topics_df$Topic <- paste("Topic", 1:num_topics)
    
    
    topic_labels <- setNames(topic_names, topics_df$Topic)
    
    p <- ggplot(topics_df, aes(x = Topic, y = CoherenceScore, fill = Topic, text = paste("Topic:", Topic, "<br>CoherenceScore:", CoherenceScore))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(name = "Topic", values = topic_colors) +
      scale_x_discrete(labels = topic_labels) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none") +
      labs(x = "Topic", y = "Coherence Score", title = "Coherence Scores of Topics")
    ggplotly(p, tooltip = "text")
  })
  
  output$wordcloud <- renderWordcloud2({
    req(input$selected_topic)
    wordcloud_data <- new_final_df
    wordcloud2(wordcloud_data, size = 1.5, shape = 'square',
               color = brewer.pal(8, "Dark2"), backgroundColor = "white")
  })
  
  output$heatmapPlot <- renderPlotly({
    
    long_data <- melt(seasonal_topic_data, id.vars = 'Season')
    
    
    p <- ggplot(long_data, aes(x = variable, y = Season, fill = value)) +
      geom_tile(color = "black") +
      scale_fill_gradient(low = "white", high = "#0072B2") +
      labs(x = "Word", y = "Season", title = "Topic Frequency by Season") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5, size = 6))
    
    
    ggplotly(p)
  })
}

# Run the integrated Shiny app
shinyApp(ui, server)





library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(plotly)
library(wordcloud2)
library(reshape2)
library(RColorBrewer)

# UI for integrated Shiny modules with additional sidebar items
ui <- dashboardPage(
  dashboardHeader(titleWidth = 290, title = "Integrated Shiny Modules"),
  dashboardSidebar(
    width = 290,
    sidebarMenu(
      menuItem("2022 State-Level Pet Data", tabName = "state_level_data", icon = icon("paw"),
               menuItem("Overall Pet Care Score", tabName = "health_scores", icon = icon("heartbeat")),
               menuItem("Vaccination Rate", tabName = "vaccination", icon = icon("syringe")),
               menuItem("Microchip Rate", tabName = "microchip", icon = icon("microchip")),
               menuItem("Rate of Adopted Pets Returned to Shelters", tabName = "return_rates", icon = icon("reply")),
               menuItem("Average Pet Spending by Households", tabName = "spending", icon = icon("wallet"))
      ),
      menuItem("Reddit Pets: NLP Data Dive", tabName = "nlp_data_dive", icon = icon("search"),
               menuItem("Topic Analysis", tabName = "topic_analysis_sub", icon = icon("comment-alt")),
               menuItem("Season Trend Analysis", tabName = "season_trend_analysis", icon = icon("calendar-alt"))
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "state_level_data"), # Placeholder for state-level data tabs
      tabItem(tabName = "nlp_data_dive"), # Placeholder for NLP data dive tabs
      tabItem(tabName = "vaccination", ui_vaccine),
      tabItem(tabName = "spending", ui_expen),
      tabItem(tabName = "return_rates", ui_return),
      tabItem(tabName = "microchip", ui_chip),
      tabItem(tabName = "health_scores", ui_score),
      tabItem(tabName = "topic_analysis_sub",
              fluidRow(
                box(
                  selectInput("selected_topic", "Select a topic:", choices = c("1. Family and Life Advice", "2. Pet Care and Daily Management", "3. Pet Health and Medical Emergencies", "4. Emotional Care and Nighttime Anxiety", "5. Pet Diet and Weight Management")),
                  width = 12
                )
              ),
              fluidRow(
                box(
                  plotlyOutput("topic_plot", height = "600px"),
                  div(id = "rationale_text", style = "margin-top: 20px;"),
                  div(style = "margin-top: 20px; text-align: justify;",
                      "This word frequency visualization is based on comments from the pet section between April 2023 and April 2024, using data cleansing and topic analysis to get each topic and its associated words, reflecting the main discussion within the community."),
                  width = 12
                )
              )
      ),
      tabItem(tabName = "season_trend_analysis",
              fluidRow(
                box(
                  plotlyOutput("heatmapPlot", height = "600px"),
                  width = 12
                )
              ),
              fluidRow(
                box(
                  p("The words that appear in every season are primarily common types of pets (such as cats, dogs), places or activities related to pets (such as veterinarians, houses, food), and common descriptive words (such as time, love, care). This indicates that these are the most common and fundamental vocabulary in pet-related topics.",
                    "Words that appear only in specific seasons may reflect particular events, topics, or trends of that quarter. For example:",
                    tags$ul(
                      tags$li("The words such as 'disease,' 'effectively,' 'Sweden' that appear in 24s2 may be related to specific pet diseases or medical topics."),
                      tags$li("The words like 'animals,' 'shelter,' and 'surgery' that appear in 24s1 may be associated with topics such as animal shelters, adoption, and medical procedures."),
                      tags$li("The words 'friend,' 'pain,' and 'update' that appear in 23s4 may be related to topics such as pet illness, injury, and updates on their condition."),
                      tags$li("The words 'advice,' 'comments,' 'emergency,' and 'life' that appear in 23s3 may be related to topics such as pet emergencies, seeking advice, and other life-related discussions."),
                      tags$li("The words 'health,' 'kitten,' 'training,' and 'turtle' that appear in 23s2 may be related to the health and training of specific pets such as kittens and turtles.")
                    ),
                    "Seasonal changes may also affect the distribution of topic words in different quarters. For example, in certain seasons, there may be more words related to weather, holidays, and seasonal diseases."
                  ),
                  width = 12
                )
              )
      )
    )
  )
)

# Server function for integrated Shiny modules with updated navigation logic
server <- function(input, output) {
  # Original server functions from module 1
  server_score(input, output)
  server_vaccine(input, output)
  server_chip(input, output)
  server_return(input, output)
  server_expen(input, output)
  
  # Additional server logic for module 2
  topic_rationale <- function() {
    list(
      "1. Family and Life Advice" = "The frequent use of words such as 'feel', 'care', 'love', and 'family' suggests that this topic is centered around emotional support and familial advice. It highlights a discourse rich in affectivity and concern for the well-being of family members, suggesting that users are seeking or offering guidance on nurturing family bonds, understanding emotional needs, and creating a supportive home environment. This pattern of word frequency reflects the community's collective emphasis on the importance of emotional care within the family dynamic.",
      "2. Pet Care and Daily Management" = "The recurring appearance of words like 'food', 'morning', and 'night' indicates a focus on daily care and pet management. These terms point to a routine-centric discussion within the community, centered on the regular aspects of pet care, including feeding schedules, morning activities, and nighttime routines. The prominence of such words in the discourse reflects a shared interest in maintaining the health and well-being of pets through consistent daily practices.",
      "3. Pet Health and Medical Emergencies" = "The prevalence of health-related terms such as 'vet', 'sick', and 'emergency' underscores the medical aspect of pet care. These keywords signal a community's engagement with topics concerning veterinary visits, illness management, and urgent care for pets, highlighting the importance placed on the health and medical needs of animals. The focus on such terms reflects a collective concern for recognizing and responding to health issues, ensuring timely professional care, and preparing for potential emergencies in pet ownership.",
      "4. Emotional Care and Nighttime Anxiety" = "The presence of words such as 'love', 'night', and 'anxiety' points to discussions around emotional support and stress management for pets. These terms suggest a compassionate approach to addressing pets' emotional well-being, dealing with nighttime restlessness, and alleviating anxiety. This pattern indicates the community's awareness of the psychological needs of pets and the desire to foster an environment that nurtures their mental health. It underscores the importance of emotional bonding and providing a calming presence for pets facing stress or discomfort.",
      "5. Pet Diet and Weight Management" = "The frequent mentions of 'food', 'diet', 'weight', and 'feeding' are associated with the topics of pet nutrition and weight management. These terms reflect a significant concern and active discussion within the community regarding the proper dietary habits, the importance of balanced food intake, and strategies for maintaining a healthy weight for pets. This focus underscores the commitment to ensuring pets receive the appropriate nutrients for their health and vitality while preventing obesity and related health issues through mindful feeding practices."
    )
  }
  
  reactive_data <- reactive({
    req(input$selected_topic)
    df <- all_topics_data %>% filter(Topic == input$selected_topic)
    df$WordReorder <- reorder(df$Word, -df$Frequency)
    return(df)
  })
  
  output$topic_plot <- renderPlotly({
    req(input$selected_topic)
    df <- reactive_data()
    topic_specific_color <- topic_colors[df$Topic[1]]
    p <- ggplot(df, aes(x = WordReorder, y = Frequency, fill = Topic, text = paste("Word:", Word, "<br>Frequency:", Frequency))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = topic_specific_color) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(size = 12, face = "bold"),
            plot.title = element_text(size = 20, face = "bold"),
            axis.title.y = element_text(size = 16, face = "bold")) +
      labs(x = "Word", y = "Frequency", title = input$selected_topic, fill = "Topic")
    ggplotly(p, tooltip = "text")
  })
  
  observeEvent(input$selected_topic, {
    req(input$selected_topic)
    rationale <- topic_rationale()[input$selected_topic]
    shinyjs::html("rationale_text", sprintf("<strong>Reason for topic naming:</strong> %s", rationale))
  })
  
  output$coherence_plot <- renderPlotly({
    req(input$selected_topic)
    
    topics_df$Topic <- paste("Topic", 1:num_topics)
    
    
    topic_labels <- setNames(topic_names, topics_df$Topic)
    
    p <- ggplot(topics_df, aes(x = Topic, y = CoherenceScore, fill = Topic, text = paste("Topic:", Topic, "<br>CoherenceScore:", CoherenceScore))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(name = "Topic", values = topic_colors) +
      scale_x_discrete(labels = topic_labels) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none") +
      labs(x = "Topic", y = "Coherence Score", title = "Coherence Scores of Topics")
    ggplotly(p, tooltip = "text")
  })
  
  output$wordcloud <- renderWordcloud2({
    req(input$selected_topic)
    wordcloud_data <- new_final_df
    wordcloud2(wordcloud_data, size = 1.5, shape = 'square',
               color = brewer.pal(8, "Dark2"), backgroundColor = "white")
  })
  
  output$heatmapPlot <- renderPlotly({
    
    long_data <- melt(seasonal_topic_data, id.vars = 'Season')
    
    
    p <- ggplot(long_data, aes(x = variable, y = Season, fill = value)) +
      geom_tile(color = "black") +
      scale_fill_gradient(low = "white", high = "#0072B2") +
      labs(x = "Word", y = "Season", title = "Topic Frequency by Season") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5, size = 6))
    
    
    ggplotly(p)
  })
}


shinyApp(ui, server)






library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(plotly)
library(wordcloud2)
library(reshape2)
library(RColorBrewer)

# Function to create a custom dashboard header with a hyperlink
# Custom Header with a hyperlink at the left side
header_with_link <- function(url, label) {
  dashboardHeader(
    title = tags$a(href = url, target = "_blank", tags$span(label, style = "font-size: 18px; color: white;"), style = "color: white; text-decoration: none;"),
    titleWidth = 430
  )
}

# UI for integrated Shiny modules with a clickable header
ui <- dashboardPage(
  header_with_link("https://github.com/QMSS-G5063-2024/Group_O_PetScape-USA.git", "Pet Data Insights : State Trends & Reddit NLP Dive"), # Replace http://www.example.com with your desired URL
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("2022 State-Level Pet Data", tabName = "state_level_data", icon = icon("paw"),
               menuItem("Overall Pet Care Score", tabName = "health_scores", icon = icon("heartbeat")),
               menuItem("Vaccination Rate", tabName = "vaccination", icon = icon("syringe")),
               menuItem("Microchip Rate", tabName = "microchip", icon = icon("microchip")),
               menuItem("Rate of Adopted Pets Returned to Shelters", tabName = "return_rates", icon = icon("reply")),
               menuItem("Average Pet Spending by Households", tabName = "spending", icon = icon("wallet"))
      ),
      menuItem("Reddit Pets: NLP Data Dive", tabName = "nlp_data_dive", icon = icon("search"),
               menuItem("Topic Analysis", tabName = "topic_analysis", icon = icon("comment-alt"),
                        menuSubItem("Topic Model", tabName = "topic_analysis_sub"),
                        menuSubItem("Word Cloud", tabName = "word_frequency")
               ),
               menuItem("Season Trend Analysis", tabName = "season_trend_analysis", icon = icon("calendar-alt"))
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    # ... (Include all the tabItems and their content as before)
    tabItems(
      tabItem(tabName = "state_level_data"), # Placeholder for state-level data tabs
      tabItem(tabName = "nlp_data_dive"), # Placeholder for NLP data dive tabs
      tabItem(tabName = "vaccination", ui_vaccine),
      tabItem(tabName = "spending", ui_expen),
      tabItem(tabName = "return_rates", ui_return),
      tabItem(tabName = "microchip", ui_chip),
      tabItem(tabName = "health_scores", ui_score),
      tabItem(tabName = "topic_analysis_sub",
              fluidRow(
                box(
                  selectInput("selected_topic", "Select a topic:", choices = c("1. Family and Life Advice", "2. Pet Care and Daily Management", "3. Pet Health and Medical Emergencies", "4. Emotional Care and Nighttime Anxiety", "5. Pet Diet and Weight Management")),
                  width = 12
                )
              ),
              fluidRow(
                box(
                  plotlyOutput("topic_plot", height = "600px"),
                  div(id = "rationale_text", style = "margin-top: 20px;"),
                  div(style = "margin-top: 20px; text-align: justify;",
                      "This word frequency visualization is based on comments from the pet section between April 2023 and April 2024, using data cleansing and topic analysis to get each topic and its associated words, reflecting the main discussion within the community."),
                  width = 12
                )
              )
      ),
      tabItem(tabName = "word_frequency",
              fluidRow(
                box(
                  wordcloud2Output("wordcloud", width = "100%", height = "600px"),
                  width = 12
                )
              )
      ),
      tabItem(tabName = "season_trend_analysis",
              fluidRow(
                box(
                  plotlyOutput("heatmapPlot", height = "600px"),
                  width = 12
                )
              ),
              fluidRow(
                box(
                  p("The words that appear in every season are primarily common types of pets (such as cats, dogs), places or activities related to pets (such as veterinarians, houses, food), and common descriptive words (such as time, love, care). This indicates that these are the most common and fundamental vocabulary in pet-related topics.",
                    "Words that appear only in specific seasons may reflect particular events, topics, or trends of that quarter. For example:",
                    tags$ul(
                      tags$li("The words such as 'disease,' 'effectively,' 'Sweden' that appear in 24s2 may be related to specific pet diseases or medical topics."),
                      tags$li("The words like 'animals,' 'shelter,' and 'surgery' that appear in 24s1 may be associated with topics such as animal shelters, adoption, and medical procedures."),
                      tags$li("The words 'friend,' 'pain,' and 'update' that appear in 23s4 may be related to topics such as pet illness, injury, and updates on their condition."),
                      tags$li("The words 'advice,' 'comments,' 'emergency,' and 'life' that appear in 23s3 may be related to topics such as pet emergencies, seeking advice, and other life-related discussions."),
                      tags$li("The words 'health,' 'kitten,' 'training,' and 'turtle' that appear in 23s2 may be related to the health and training of specific pets such as kittens and turtles.")
                    ),
                    "Seasonal changes may also affect the distribution of topic words in different quarters. For example, in certain seasons, there may be more words related to weather, holidays, and seasonal diseases."
                  ),
                  width = 12
                )
              )
      )
    )
  )
)

# Server function for integrated Shiny modules with updated navigation logic
server <- function(input, output) {
  # Original server functions from module 1
  server_score(input, output)
  server_vaccine(input, output)
  server_chip(input, output)
  server_return(input, output)
  server_expen(input, output)
  
  # Additional server logic for module 2
  topic_rationale <- function() {
    list(
      "1. Family and Life Advice" = "The frequent use of words such as 'feel', 'care', 'love', and 'family' suggests that this topic is centered around emotional support and familial advice. It highlights a discourse rich in affectivity and concern for the well-being of family members, suggesting that users are seeking or offering guidance on nurturing family bonds, understanding emotional needs, and creating a supportive home environment. This pattern of word frequency reflects the community's collective emphasis on the importance of emotional care within the family dynamic.",
      "2. Pet Care and Daily Management" = "The recurring appearance of words like 'food', 'morning', and 'night' indicates a focus on daily care and pet management. These terms point to a routine-centric discussion within the community, centered on the regular aspects of pet care, including feeding schedules, morning activities, and nighttime routines. The prominence of such words in the discourse reflects a shared interest in maintaining the health and well-being of pets through consistent daily practices.",
      "3. Pet Health and Medical Emergencies" = "The prevalence of health-related terms such as 'vet', 'sick', and 'emergency' underscores the medical aspect of pet care. These keywords signal a community's engagement with topics concerning veterinary visits, illness management, and urgent care for pets, highlighting the importance placed on the health and medical needs of animals. The focus on such terms reflects a collective concern for recognizing and responding to health issues, ensuring timely professional care, and preparing for potential emergencies in pet ownership.",
      "4. Emotional Care and Nighttime Anxiety" = "The presence of words such as 'love', 'night', and 'anxiety' points to discussions around emotional support and stress management for pets. These terms suggest a compassionate approach to addressing pets' emotional well-being, dealing with nighttime restlessness, and alleviating anxiety. This pattern indicates the community's awareness of the psychological needs of pets and the desire to foster an environment that nurtures their mental health. It underscores the importance of emotional bonding and providing a calming presence for pets facing stress or discomfort.",
      "5. Pet Diet and Weight Management" = "The frequent mentions of 'food', 'diet', 'weight', and 'feeding' are associated with the topics of pet nutrition and weight management. These terms reflect a significant concern and active discussion within the community regarding the proper dietary habits, the importance of balanced food intake, and strategies for maintaining a healthy weight for pets. This focus underscores the commitment to ensuring pets receive the appropriate nutrients for their health and vitality while preventing obesity and related health issues through mindful feeding practices."
    )
  }
  
  topic_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")
  
  reactive_data <- reactive({
    req(input$selected_topic)
    df <- all_topics_data %>% filter(Topic == input$selected_topic)
    print(df$Topic[1])  # 添加这一行来检查值
    df$WordReorder <- reorder(df$Word, -df$Frequency)
    return(df)
  })
  
  output$topic_plot <- renderPlotly({
    req(input$selected_topic)
    df <- reactive_data()
    topic_specific_color <- topic_colors[df$Topic[1]]
    p <- ggplot(df, aes(x = WordReorder, y = Frequency, fill = Topic, text = paste("Word:", Word, "<br>Frequency:", Frequency))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = topic_specific_color) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(size = 12, face = "bold"),
            plot.title = element_text(size = 20, face = "bold"),
            axis.title.y = element_text(size = 16, face = "bold")) +
      labs(x = "Word", y = "Frequency", title = input$selected_topic, fill = "Topic")
    ggplotly(p, tooltip = "text")
  })
  
  observeEvent(input$selected_topic, {
    req(input$selected_topic)
    rationale <- topic_rationale()[input$selected_topic]
    shinyjs::html("rationale_text", sprintf("<strong>Reason for topic naming:</strong> %s", rationale))
  })
  
  output$wordcloud <- renderWordcloud2({
    req(input$selected_topic)
    wordcloud_data <- new_final_df
    wordcloud2(wordcloud_data, size = 1.5, shape = 'square',
               color = brewer.pal(8, "Dark2"), backgroundColor = "white")
  })
  
  output$heatmapPlot <- renderPlotly({
    
    long_data <- melt(seasonal_topic_data, id.vars = 'Season')
    
    
    p <- ggplot(long_data, aes(x = variable, y = Season, fill = value)) +
      geom_tile(color = "black") +
      scale_fill_gradient(low = "white", high = "#0072B2") +
      labs(x = "Word", y = "Season", title = "Topic Frequency by Season") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5, size = 6))
    
    
    ggplotly(p)
  })
}

# Run the integrated Shiny app
shinyApp(ui, server)