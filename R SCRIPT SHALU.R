# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)

# Load the dataset
data <- read.csv("C:/Users/Priyal/Dropbox/My PC (LAPTOP-ND4LQUT1)/Downloads/reviews starbucks data.csv")
View(data)

# 1. Structure and summary of the data
str(data)
summary(data)

# 2. Handling missing values
data <- na.omit(data) # Remove rows with missing values

# 1. Distribution of review ratings
Rating_counts <- data %>% group_by(Rating) %>% summarise(count = n())
ggplot(Rating_counts, aes(x = factor(Rating), y = count, fill = factor(Rating))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5) +
  labs(title = "Distribution of Review Ratings", x = "Rating", y = "Count") +
  theme_minimal()

# 2. Percentage of review ratings (Pie chart)
Rating_counts <- Rating_counts %>% mutate(percentage = count / sum(count) * 100)
ggplot(Rating_counts, aes(x = "", y = percentage, fill = factor(Rating))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Percentage of Review Ratings", fill = "Rating") +
  theme_void()

#3. Distribution of review lengths (Histogram)
Review_lengths <- nchar(data$Review)
ggplot(data, aes(x = Review_lengths)) +
  geom_histogram(binwidth = 20, fill = "steelblue", color = "black") +
  geom_text(stat = 'bin', binwidth = 20, aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribution of Review Lengths", x = "Review Length", y = "Count") +
  theme_minimal()


#4.Correlation between review length and rating
Review_lengths <- nchar(data$Review)
cor_data <- data.frame(Rating = data$Rating, Review_length = Review_lengths)
ggplot(cor_data, aes(x = Review_length, y = Rating)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Correlation Between Review Length and Rating", x = "Review Length", y = "Rating") +
  theme_minimal()


# Count reviews with and without images
Image_counts <-data %>%
  mutate(Has_Image = ifelse(Image_Links == "['No Images']", "No", "Yes")) %>%
  count(Has_Image)

#5. Bar plot for image counts
ggplot(Image_counts, aes(x = Has_Image, y = n, fill = Has_Image)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Reviews with and without Images", x = "Has Image", y = "Count")

# 6. Histogram of Review Lengths
  review_lengths <- data %>% mutate(Review_Length = nchar(as.character(Review)))
ggplot(review_lengths, aes(x = Review_Length)) +
  geom_histogram(binwidth = 20, fill = "orange", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribution of Review Lengths", x = "Review Length", y = "Count") +
  theme_minimal()

# 7. Top 10 Most Frequent Reviewers
reviewer_summary <- data %>% 
  group_by(name) %>% 
  summarize(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  slice(1:10)
ggplot(reviewer_summary, aes(x = reorder(name, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "pink") +
  coord_flip() +
  geom_text(aes(label = Count), hjust = -0.2) +
  labs(title = "Top 10 Reviewers", x = "Reviewer Name", y = "Count") +
  theme_minimal()

# 8. Hourly Review Distribution
hourly_reviews <- data %>% 
  mutate(Hour = hour(hms(substr(Date, 12, 19)))) %>% 
  group_by(Hour) %>% 
  summarize(Count = n())
ggplot(hourly_reviews, aes(x = Hour, y = Count)) +
  geom_bar(stat = "identity", fill = "green") +
  geom_text(aes(label = Count), vjust = -0.5) +
  labs(title = "Hourly Distribution of Reviews", x = "Hour of Day", y = "Count") +
  theme_minimal()

# 9. Comparison of Ratings for Users with and Without Images
rating_image <- data %>% 
  mutate(Has_Image = ifelse(Image_Links == "['No Images']", "No", "Yes")) %>% 
  group_by(Has_Image) %>% 
  summarize(Average_Rating = mean(Rating, na.rm = TRUE))
ggplot(rating_image, aes(x = Has_Image, y = Average_Rating, fill = Has_Image)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Average_Rating, 2)), vjust = -0.5) +
  labs(title = "Comparison of Ratings for Users with and Without Images", x = "Has Image", y = "Average Rating") +
  theme_minimal()

# 10. Review Length vs. Rating (Categorical)
length_rating <- data %>% mutate(Review_Length_Category = case_when(
  nchar(Review) < 50 ~ "Short",
  nchar(Review) <= 200 ~ "Medium",
  TRUE ~ "Long"
))
ggplot(length_rating, aes(x = Review_Length_Category, y = Rating, fill = Review_Length_Category)) +
  geom_boxplot() +
  labs(title = "Review Length vs. Rating", x = "Review Length Category", y = "Rating") +
  theme_minimal()

# 11. Average Review Length by Rating
avg_length_by_rating <- length_rating %>% 
  group_by(Rating) %>% 
  summarize(Average_Length = mean(nchar(Review), na.rm = TRUE))
ggplot(avg_length_by_rating, aes(x = factor(Rating), y = Average_Length)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = round(Average_Length, 1)), vjust = -0.5) +
  labs(title = "Average Review Length by Rating", x = "Rating", y = "Average Length") +
  theme_minimal()

# 12. Percentage of Positive vs Negative Reviews
sentiment_distribution <- data %>% 
  mutate(Sentiment = ifelse(Rating >= 4, "Positive", "Negative")) %>% 
  group_by(Sentiment) %>% 
  summarize(Count = n()) %>% 
  mutate(Percentage = Count / sum(Count) * 100)
ggplot(sentiment_distribution, aes(x = "", y = Percentage, fill = Sentiment)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Percentage of Positive vs Negative Reviews", fill = "Sentiment") +
  theme_minimal()

#13 Top 10 Locations by Number of Reviews
locations <- data %>%
  count(location, sort = TRUE) %>%
  top_n(10)

ggplot(locations, aes(x = reorder(location, n), y = n)) +
  geom_bar(stat = "identity", fill = "green") +
  geom_text(aes(label = n), hjust = -0.3) +
  coord_flip() +
  labs(title = "Top 10 Locations with Most Reviews", x = "Location", y = "Count")

# 14. Top 10 Locations with Most Positive Reviews
positive_reviews <- data %>% http://127.0.0.1:47553/graphics/5c7e2c51-a352-4d45-bbd0-007a6f7aac63.png
  filter(Rating >= 4) %>% 
  group_by(location) %>% 
  summarize(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  slice(1:10)
ggplot(positive_reviews, aes(x = reorder(location, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  geom_text(aes(label = Count), hjust = -0.2) +
  labs(title = "Top 10 Locations with Most Positive Reviews", x = "Location", y = "Count") +
  theme_minimal()


