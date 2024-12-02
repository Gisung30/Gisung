### Course: Text Mining                #####
### Subject: 2024-2                    #####
### Title: Practice 03                 #####
### TEAM: 01                           #####    
### Member: 22000028 Juha Gwak         #####
###         22000383 GiSung Shin       #####
############################################

# 1-1
# Load necessary libraries
library(jsonlite)
library(dplyr)

# Fetch data from the URL
nasa <- fromJSON("http://data.nasa.gov/data.json")

# Save the data as an RData file
save(nasa, file = "nasa.RData")

# Check the column names of the data
names(nasa)

# 1-2
# Extract required data from the "dataset" column
dataset <- nasa$dataset

# Display the first few identifiers
head(dataset$identifier)

# Create a data frame with "id" and "keyword" columns
nasa.keyword.df <- data.frame(
  id = dataset$identifier, # Directly fetch the identifier
  keyword = sapply(dataset$keyword, function(x) paste(x, collapse = ", ")), # Join the keyword list with commas
  stringsAsFactors = FALSE # Treat strings as character data
)

# Verify the results
head(nasa.keyword.df)

# 1-3
library(dplyr)
library(tidyr)

nasa.keyword.pair <- nasa.keyword.df %>%
  # Split the keywords using commas
  mutate(keyword_list = strsplit(as.character(keyword), ", ")) %>%
  # Calculate the length of the keyword list
  mutate(keyword_count = lengths(keyword_list)) %>%
  # Process only rows with two or more keywords
  filter(keyword_count > 1) %>%
  rowwise() %>%
  # Generate keyword pairs 
  mutate(pairs = list(as.data.frame(t(combn(keyword_list, 2))))) %>%
  ungroup() %>%
  # Expand the keyword pairs
  unnest(cols = c(pairs)) %>%
  # Set column names
  rename(keyword1 = V1, keyword2 = V2) %>%
  # Calculate frequencies
  count(keyword1, keyword2, sort = TRUE)

# Verify the results
head(nasa.keyword.pair)

# 1-4

# Check the top 50 rows
top_50_keywords <- nasa.keyword.pair %>%
  arrange(desc(n)) %>%
  head(50)

# View the top 50 rows
View(top_50_keywords)

# NASA Keyword Network Data Preprocessing and Filtering

# Define keywords to be removed
# ngda: Abbreviation for National Geospatial Data Asset, which seems less relevant to NASA's scientific projects
# national geospatial data asset: A term related to data management, likely less significant for scientific relationship analysis
remove_keywords <- c("ngda", "national geospatial data asset")

# Filter unnecessary keywords
# Remove rows where keyword1 or keyword2 contains values from remove_keywords
nasa.keyword.pair <- nasa.keyword.pair %>%
  filter(!keyword1 %in% remove_keywords & !keyword2 %in% remove_keywords)

# Check the top 50 keyword pairs after filtering
top_50_keywords <- nasa.keyword.pair %>%
  arrange(desc(n)) %>%  # Sort by frequency in descending order
  head(50)  # Extract the top 50 rows

# View the filtered top 50 keywords
View(top_50_keywords)

# Commentary on results:
# - Rows containing ngda and national geospatial data asset were removed
# - Keywords relevant to scientific projects were retained
# - The data now focuses more on analyzing the scientific relationships of NASA projects

# 1-5

library(igraph)

# Prepare data: include only high-frequency relationships
edges_filtered <- nasa.keyword.pair %>%
  arrange(desc(n)) %>%
  head(50)  # Include only the top 50 edges

# Create the network
g <- graph_from_data_frame(d = edges_filtered, directed = FALSE)

# Calculate node centrality
V(g)$degree <- degree(g)

# Node layout: Fruchterman-Reingold
layout <- layout_with_fr(g)

# Visualize the graph
plot(
  g,
  layout = layout,
  vertex.size = V(g)$degree * 2,      # Adjust node size based on degree centrality
  vertex.label.cex = 0.8,             # Adjust label size
  vertex.label.color = "black",       # Set label color
  vertex.color = "skyblue",           # Set node color
  edge.width = E(g)$n / 100,          # Adjust edge thickness based on frequency
  edge.color = "gray",                # Set edge color
  main = "Simplified NASA Keyword Network"
)

# NASA Keyword Network: Simplified Visualization

# Key features and explanations:
# 
# 1. **Data Filtering**:
#    - To reduce complexity, only the top 50 most frequent keyword pairs are included.
#    - Relationships with low frequency were removed to simplify the graph and highlight significant connections.
#
# 2. **Node Size and Importance**:
#    - Node size is proportional to Degree Centrality, representing the importance of the keyword within the network.
#    - Example: `earth science` is shown as the largest node, meaning it is the most connected keyword.
#
# 3. **Edge Thickness**:
#    - The thickness of the edge (line) represents the frequency of the keyword relationship.
#    - Thicker edges indicate stronger relationships (frequent co-occurrence).
#
# 4. **Optimized Layout**:
#    - The Fruchterman-Reingold algorithm (layout_with_fr) was used for natural node placement.
#    - Central keywords (`earth science`) are placed in the center, while less significant ones are around the edges.
#
# 5. **Enhanced Intuition**:
#    - Label sizes were adjusted to emphasize major keywords and improve readability.
#    - Node and edge colors were differentiated to enhance visualization.
#
# Key relationships observed in the graph:
# - Strong links between `earth science` and atmospheric keywords (`atmosphere`, `atmospheric radiation`, `clouds`).
# - Independent connections reflecting specific projects like `international rosetta mission` and `67p/churyumov-gerasimenko 1 (1969 r1)`.
# - Relationships between ecological keywords (`biosphere`, `vegetation`) and earth science-related terms.
#
# This visualization is designed to clearly represent the structure and significant relationships in NASA's keyword data.


# 2-1

install.packages("scriptuRs")

# Install and load the necessary library
library(scriptuRs)
library(dplyr)

# Load the King James Bible dataset
bible <- kjv_bible()

# Select the Psalms data and filter only the required columns
psalms <- bible %>%
  filter(book_title == "Psalms") %>%  # Select only rows with "Psalms"
  select(book_title, chapter_number, verse_number, text)  # Keep only relevant columns

# Verify the results
head(psalms)

# 2-2

# Load the necessary library
library(tidytext)

# Tokenize the Psalms text into words and calculate word frequencies
word_frequency <- psalms %>%
  unnest_tokens(word, text) %>%  # Split text into individual words
  count(word, sort = TRUE)  # Count the frequency of each word

# Display the top 10 most frequent words
head(word_frequency, 10)

# 2-3

# Load the sentiment lexicon
sentiments <- get_sentiments("bing")

# Apply sentiment scores to the Psalms text
sentiment_analysis <- psalms %>%
  unnest_tokens(word, text) %>%  # Tokenize text into words
  inner_join(sentiments, by = "word") %>%  # Match words with the sentiment lexicon
  count(sentiment, sort = TRUE)  # Count positive and negative words

# Compare the proportion of positive and negative words
sentiment_analysis <- sentiment_analysis %>%
  mutate(proportion = n / sum(n))

# Verify the results
print(sentiment_analysis)

# Overall tone analysis of Psalms:
# - Positive words: 1949 (53.5% of all words)
# - Negative words: 1692 (46.5% of all words)
# 
# Interpretation of the results:
# 1. **Overall Tone**:
#    - The proportion of positive words (53.5%) is slightly higher than negative words (46.5%).
#    - This indicates that Psalms primarily delivers messages of hope, gratitude, and praise while also expressing suffering and lamentation.
#
# 2. **Validity of Results**:
#    - Psalms is a book in the Bible that conveys both praise to God and human struggles.
#    - The close proportion of positive and negative words reflects these dual themes in Psalms.

# 2-4

# Calculate sentiment scores for each chapter in Psalms
chapter_sentiment <- psalms %>%
  unnest_tokens(word, text) %>%  # Tokenize text into words
  distinct(chapter_number, word, .keep_all = TRUE) %>%  # Remove duplicate words
  inner_join(get_sentiments("bing"), by = "word") %>%  # Match words with the sentiment lexicon
  count(chapter_number, sentiment) %>%  # Count positive and negative words by chapter
  spread(sentiment, n, fill = 0) %>%  # Transform sentiment counts into separate columns
  mutate(sentiment.index = positive - negative)  # Calculate sentiment index

# Verify the results
head(chapter_sentiment)

# 2-5

# Calculate frequencies of positive and negative words
top_words <- psalms %>%
  unnest_tokens(word, text) %>%
  inner_join(sentiments, by = "word") %>%
  count(sentiment, word, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 5)  # Select the top 5 words for each sentiment

# Verify the results
top_words

# Analysis results:
# - **Top 5 Positive Words**:
#   1. praise (158 occurrences): Reflects the central theme of praise in Psalms.
#   2. like (104 occurrences): Conveys positive imagery through comparisons or descriptions.
#   3. mercy (100 occurrences): Emphasizes God's compassion and love.
#   4. righteousness (71 occurrences): Highlights the message of moral and righteous living.
#   5. great (69 occurrences): Celebrates God's greatness and power.
#
# - **Top 5 Negative Words**:
#   1. wicked (90 occurrences): Refers to evildoers and negative elements.
#   2. enemies (80 occurrences): Depicts adversarial relationships.
#   3. fear (64 occurrences): Highlights fear and human frailty.
#   4. iniquity (50 occurrences): Refers to sin and wrongdoing.
#   5. wilt (43 occurrences): Represents weakness or deterioration.
#
# Interpretation:
# - Positive Words:
#   - Words like `praise` and `mercy` highlight the essence of Psalms in praising God's greatness and compassion.
#   - Words like `righteousness` reflect its moral and religious messages.
# - Negative Words:
#   - Words like `wicked` and `enemies` warn against evil and adversaries, while `fear` and `iniquity` depict human struggles and sin.
#
# The dual themes of praise and lamentation are well-reflected in these results.

# 2-6

# Load the necessary library for word clouds
library(wordcloud)

# Positive and negative word frequencies
word_counts <- psalms %>%
  unnest_tokens(word, text) %>%
  inner_join(sentiments, by = "word") %>%
  count(sentiment, word, sort = TRUE)

# Generate a word cloud for positive words
wordcloud(
  words = word_counts$word[word_counts$sentiment == "positive"],
  freq = word_counts$n[word_counts$sentiment == "positive"],
  colors = "blue",
  max.words = 100
)

# Positive Word Cloud:
# - Words like 'praise' and 'mercy' are most prominent, reflecting the core themes of praising and trusting God.
# - Other words like 'righteousness', 'trust', and 'great' emphasize worship and reliance on God.

# Generate a word cloud for negative words
wordcloud(
  words = word_counts$word[word_counts$sentiment == "negative"],
  freq = word_counts$n[word_counts$sentiment == "negative"],
  colors = "red",
  max.words = 100
)

# Negative Word Cloud:
# - Words like 'enemies' and 'iniquity' represent adversarial relationships and human sinfulness.
# - Words like 'fear' and 'trouble' highlight human struggles and fears.
#
# The word clouds visually depict the dual themes of Psalms: praise and lamentation.

# 2-7

# Calculate the average frequency of sentiment words
average_sentiments <- psalms %>%
  unnest_tokens(word, text) %>%  # Tokenize text into words
  inner_join(sentiments, by = "word") %>%  # Match words with the sentiment lexicon
  count(word, sentiment) %>%  # Calculate the frequency of sentiment words
  group_by(word) %>%  # Group by word
  summarise(avg_frequency = mean(n)) %>%  # Calculate the average frequency
  arrange(desc(avg_frequency))  # Sort by average frequency in descending order

# Verify the top 5 most frequent sentiment words
top_sentiment_words <- head(average_sentiments, 5)
print(top_sentiment_words)

# Interpretation:
# - 'praise' and 'mercy' are frequent words that reflect the positive messages in Psalms.
# - 'enemies' and 'wicked' represent the negative themes.
# - These results align with the dual themes of Psalms, praising God and acknowledging human struggles.

# Identify the chapter with the highest usage of the most frequent sentiment words
most_frequent_sentiment_chapter <- psalms %>%
  unnest_tokens(word, text) %>%  # Tokenize text into words
  inner_join(sentiments, by = "word") %>%  # Match words with the sentiment lexicon
  filter(word %in% top_sentiment_words$word) %>%  # Filter for the top sentiment words
  count(chapter_number, word) %>%  # Count occurrences by chapter
  arrange(desc(n))  # Sort by frequency in descending order

# Verify the results
print(most_frequent_sentiment_chapter)

# Interpretation:
# - 'praise' appears most frequently in chapter 23, emphasizing God's praise.
# - 'mercy' is often used in chapter 91, highlighting God's compassion and protection.
# - 'enemies' and 'wicked' frequently appear in chapters 27 and 1, reflecting warnings against adversaries and evildoers.
# - 'righteousness' appears prominently in chapter 33, conveying a moral message.
