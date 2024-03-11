### Final Poster presentation
### Ajinkya Gaddime

Netflix <- file.choose()  
Netflix <-read.csv(Netflix,sep=",", header=TRUE)
View(Netflix)

Netflix <- Netflix %>%
  mutate(year_added = format(as.Date(date_added, format = "%B %d, %Y"), "%Y"))
##########################

### COUNTRY WISE BAR PLOT

country_counts <- count(filter(Netflix, !is.na(country)), country)
country_counts <- arrange(country_counts, desc(n))
country_counts <- top_n(country_counts, 10)
                            
View(country_counts)

ggplot(country_counts, aes(x = reorder(country, -n), y = n)) +
  geom_col(fill = "red") +
  labs(title = "Top 10 Countries on Netflix", x = "Country", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################################

#### split by movie and tv show 

type_counts <- Netflix %>%
  group_by(type) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

custom_colors <- c("black", "red")

ggplot(type_counts, aes(x = "", y = count, fill = type)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(y = count / 2, label = paste0(round(percentage), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4) +
  coord_polar("y") +
  labs(title = "Distribution of Movies and TV Shows on Netflix") +
  theme_void() +
  theme(legend.position = "right") +
  scale_fill_manual(values = custom_colors)
#####################
type_counts <- Netflix %>%
  group_by(type) %>%
  summarise(count = n())
custom_colors <- c("black", "red")
ggplot(type_counts, aes(x = "", y = count, fill = type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Distribution of Movies and TV Shows on Netflix") +
  theme_void() +
  theme(legend.position = "right") +
  scale_fill_manual(values = custom_colors)


#####################

##add date

netflix_data <- Netflix %>%
  filter(!is.na(date_added), type %in% c("Movie", "TV Show"))



additions_over_years <- netflix_data %>%
  group_by(year_added, type) %>%
  summarise(count = n()) %>%
  arrange(year_added)
custom_colors <- c("red", "black")

ggplot(additions_over_years, aes(x = year_added, y = count, color = type,fill = count)) +
  geom_line() +
  labs(title = "Netflix Movie and Series Additions Over Years",
       x = "Year Added",
       y = "Count",
       color = "Type") +
  theme_minimal() +   scale_color_manual(values = custom_colors)

###################################

filtered_data <- Netflix %>%
  filter(type %in% c("TV Show", "Movie"))

country_counts <- filtered_data %>%
  group_by(country, type) %>%
  summarise(count = n())

country_combined <- country_counts %>%
  group_by(country) %>%
  summarise(count = sum(count))

country_combined <- country_combined %>%
  arrange(desc(count))

top_10_countries <- country_combined %>%
  head(10)

ggplot(top_10_countries, aes(x = reorder(country, count), y = count, fill = count)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  labs(title = "Combined TV Shows and Movies Released (Top 10 Countries)",
       x = "Country",
       y = "Count",
       fill = "Type") +
  scale_fill_manual(values = c("TV Show" = "blue", "Movie" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
######################################################################
filtered_data <- netflix_data %>%
  filter(type %in% c("TV Show", "Movie"))

country_counts <- filtered_data %>%
  group_by(country, type) %>%
  summarise(count = n())

country_counts <- country_counts %>%
  arrange(country, desc(count))

 top_10_countries <- country_counts %>%
  group_by(country) %>%
  slice_max(order_by = count, n = 1) %>%
  arrange(desc(count))%>%
  head(10)

 top_10_summary <- aggregate(count ~ country + type, data = top_10_countries, sum)
 top_10_summary <- reshape(top_10_summary, idvar = "country", timevar = "type", direction = "wide")
##################################################
 
 movies_data <- Netflix %>%
   filter(type == "Movie")
 
 ggplot(movies_data, aes(x = release_year, fill = year_added)) +
   geom_histogram(binwidth = 1, alpha = 0.7) +
   geom_density(color = "red", alpha = 0.5) +
   labs(title = "Movie Release Year and Year Added to Netflix",
        x = "Year",
        y = "Count",
        fill = "Year Added") +
   scale_fill_gradient(low = "blue", high = "green") +
   theme_minimal()
################################################################
 country_counts <- Netflix %>%
   group_by(country) %>%
   summarise(count = n())
 
 # Sort the data by count in descending order
 country_counts <- country_counts %>%
   arrange(desc(count))
 
 # Create the bar chart
 ggplot(country_counts, aes(x = reorder(country, count), y = count)) +
   geom_bar(stat = "identity", fill = Netflix$type) +
   labs(title = "Distribution of Content by Country",
        x = "Country",
        y = "Count") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 
##############################
 netflix_data <- Netflix %>% filter(!is.na(country))
 
 # Count the number of entries for each country
 country_counts <- netflix_data %>%
   count(country, name = "count") %>%
   arrange(desc(count))
 
 # Load world map data
 world_map <- map_data("world")
 
 # Merge the country counts with the world map data
 world_data <- left_join(world_map, country_counts, by = c("region" = "country"))
 
 # Create a map based on the content added by country
 ggplot(world_data, aes(x = long, y = lat, group = group, fill = count)) +
   geom_polygon(color = "white", size = 0.5) +
   labs(title = "Netflix Content Distribution by Country",
        fill = "Count") +
   scale_fill_gradient(low = "red", high = "black") +
   theme_void()
######################################################
 
 movies_data <- Netflix %>%
   filter(type == "Movie" & !is.na(duration) & !is.na(release_year)) 
 movies_data$duration <- as.numeric(sub("min", "", movies_data$duration))
 ggplot(movies_data, aes(x = release_year, y = duration)) +
   geom_point(color = "red") +
   labs(title = "Relationship Between Movie Duration and Release Year",
        x = "Release Year",
        y = "Duration (minutes)") +
   theme_minimal()
############################################################

 netflix_data <- NEtflix %>%
   mutate(listed_in = strsplit(listed_in, ", ")) %>%
   unnest(listed_in)
 
 netflix_data <- netflix_data %>%
   filter(type %in% c("Movie", "TV Show"))
 
 genre_counts <- netflix_data %>%
   group_by(type, listed_in) %>%
   summarise(count = n())
 
 count_combinations <- Netflix %>%
   group_by(listed_in, type) %>%
   summarise(count = n())
 
 print(count_combinations)
 
 ggplot(count_combinations, aes(x = listed_in, y = count)) +
   geom_bar(stat = "identity", position = "stack", width = 0.7) +
   labs(title = "Content Distribution by Genre (Movies and TV Shows)",
        x = "Genre",
        y = "Count") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
###################################################
 ggplot(Netflix, aes(x = year_added)) +
   geom_area(aes(y = ..density..), fill = "skyblue", alpha = 0.3) +
   labs(title = "Line Plot with Filled Area Below",
        x = "Date Added",
        y = "Density") +
   theme_minimal()
###############################################################
 content_counts <- Netflix %>%
   group_by(listed_in) %>%
   summarise(count = n())
 
 ggplot(content_counts, aes(x = listed_in, y = count, fill = listed_in)) +
   geom_bar(stat = "identity") +
   labs(title = "Netflix Content Distribution by Type", x = "Content Type", y = "Count") +
   theme_minimal()
######################################################################### 
 rating_counts <- netflix_data %>%
   count(rating) %>%
   arrange(desc(n))
 ggplot(netflix_data, aes(x = reorder(rating, -table(rating)[rating]), fill = "red")) +
   geom_bar(color = "black") +
   labs(title = "Distribution of Content Ratings on Netflix",
        x = "Rating",
        y = "Count") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
   geom_text(data = rating_counts, aes(label = n, y = n), vjust = -0.5, size = 3)

##############################################################################
 ggplot(Netflix, aes(x = as.numeric(year_added))) +
   geom_density(fill = "skyblue", alpha = 0.7) +
   labs(title = "Density Plot of Content Added Yearly on Netflix",
        x = "Year",
        y = "Density") +
   theme_minimal()
##########################
Netflix$date_added <- as.Date(Netflix$date_added, format = "%B %d, %Y")
 all_years <- unique(year(netflix_data$date_added))
 ggplot(Netflix, aes(x = date_added)) +
   geom_density(fill = "red", alpha = 0.3) +
   labs(title = "Density Plot of Content Growth on Netflix",
        x = "Date Added",
        y = "Density") +
   theme_minimal() 
 #####################
 
 country_data <- data.frame(
   country = all(Netflix$country),
   content_count = table(Netflix$country)[match(unique(Netflix$country),Netflix$country)]
 )
 
 
 world_map <- map_data("world")
 world_data <- left_join(world_map, country_data, by = c("region" = "country"))

 ggplot(world_data, aes(x = long, y = lat, group = group, fill = content_count)) +
   geom_polygon(color = "white") +
   scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Content Count") +
   labs(title = "Content Added by Country on Netflix") +
   theme_minimal() 
 