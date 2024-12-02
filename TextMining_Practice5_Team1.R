### Course: Text Mining                #####
### Subject: 2024-2                    #####
### Title: Practice 03                 #####
### TEAM: 01                           #####    
### Member: 22000028 Juha Gwak         #####
###         22000383 GiSung Shin       #####
############################################


# 1-1

# Load the CSV file into the variable ct.tweet
ct.tweet <- read.csv("clintontrump_tweets.csv")

# Remove unnecessary columns 'entities' and 'extended_entities'
# Here, only the tweet text is needed, so we remove 'entities' and 'extended_entities', which contain additional data like media information.
ct.tweet <- ct.tweet[ , !(names(ct.tweet) %in% c("entities", "extended_entities"))]

# Create a new column 'candidate' based on the 'handle' column, categorizing tweets into "HillaryClinton" or "DonaldTrump"
# The ifelse function assigns "HillaryClinton" if the 'handle' value is "HillaryClinton", otherwise assigns "DonaldTrump".
ct.tweet$candidate <- ifelse(ct.tweet$handle == "HillaryClinton", "HillaryClinton", "DonaldTrump")

# 1-2

# Text preprocessing
library(stringr)

# Remove URLs from the text column
# URLs are unnecessary information that may interfere with analysis, so they are removed using a regular expression to detect "http" or "www" at the start.
ct.tweet$text <- str_remove_all(ct.tweet$text, "http\\S+|www\\S+")

# Remove expressions following the '@' symbol (mentions)
# Removing user mentions allows the analysis to focus on the main text content.
ct.tweet$text <- str_remove_all(ct.tweet$text, "@\\w+")

# Remove hashtags
# Hashtags are also considered noise that can interfere with the analysis, so words starting with "#" are removed.
ct.tweet$text <- str_remove_all(ct.tweet$text, "#\\w+")

# Replace newline characters (\n) with spaces
# Newline characters can disrupt the flow of the text, so they are replaced with spaces to maintain sentence continuity.
ct.tweet$text <- str_replace_all(ct.tweet$text, "\n", " ")

# Remove numbers
# Numbers are often meaningless in tweets, so they are removed.
ct.tweet$text <- str_remove_all(ct.tweet$text, "\\d+")

# 1-3

# Install the 'stringi' package if not already installed
# install.packages("stringi")

library(stringi)

# Define a function to remove emojis from text
rm_emoji <- function(text) {
  # Define the unicode range for emojis
  # A regular expression that includes the Unicode range for emojis is used to detect and remove emojis.
  emoji_pattern <- "[\U{1F600}-\U{1F64F}\U{1F300}-\U{1F5FF}\U{1F680}-\U{1F6FF}\U{2600}-\U{26FF}\U{2700}-\U{27BF}-\U{1F1FA}]"
  # stri_replace_all_regex function removes the emojis that match the pattern from the text.
  text <- stri_replace_all_regex(text, emoji_pattern, "")
  return(text)
}

# Apply the rm_emoji function to the 'text' column
# The sapply function applies the rm_emoji function to each element of the 'text' column to remove emojis.
ct.tweet$text <- sapply(ct.tweet$text, rm_emoji)

# 1-4

# Filter out retweets, then separate text data for Hillary Clinton and Donald Trump
# Only tweets with 'is_retweet' set to "False" are used to analyze original tweets.
hc.vec <- ct.tweet$text[ct.tweet$candidate == "HillaryClinton" & ct.tweet$is_retweet == "False"]
dt.vec <- ct.tweet$text[ct.tweet$candidate == "DonaldTrump" & ct.tweet$is_retweet == "False"]

# Combine Hillary and Trump tweet vectors to create a VCorpus
# The VCorpus function in the tm package is used to create a corpus (text collection) from the text data. 
# This step is necessary for text mining preprocessing.
library(tm)
ct.tweet.corpus <- VCorpus(VectorSource(c(hc.vec, dt.vec)))

# 1-5

# Clean and preprocess the text in the corpus
# Each tm_map function performs the specified preprocessing task, including converting to lowercase, removing stopwords, punctuation, numbers, and applying stemming.
ct.tweet.cleaned <- tm_map(ct.tweet.corpus, content_transformer(tolower))    # Convert to lowercase
ct.tweet.cleaned <- tm_map(ct.tweet.cleaned, removeWords, stopwords("en"))   # Remove stopwords
ct.tweet.cleaned <- tm_map(ct.tweet.cleaned, removePunctuation)              # Remove punctuation
ct.tweet.cleaned <- tm_map(ct.tweet.cleaned, removeNumbers)                  # Remove numbers
ct.tweet.cleaned <- tm_map(ct.tweet.cleaned, stemDocument)                   # Apply stemming (keep only root of words)

# 1-6

# Create Document-Term Matrix (DTM) and Term-Document Matrix (TDM)
# DocumentTermMatrix represents the frequency of each word in each document, while TermDocumentMatrix represents the frequency of each document for each word.
ct.tweet.dtm <- DocumentTermMatrix(ct.tweet.cleaned)
ct.tweet.tdm <- TermDocumentMatrix(ct.tweet.cleaned)

# Check the matrices to identify sparsity issues
ct.tweet.dtm
ct.tweet.tdm

# Explanation on sparsity:
# Sparse matrices contain many zero values, leading to inefficiency in storage and computation.
# To address this, infrequent words can be removed, or dimensionality reduction techniques (e.g., PCA, LSA) can be used.
# For example, we could recreate the matrices using only words with a frequency of 5 or more.

# Additional explanation:
# Sparse matrices occur when many words do not appear in each document, resulting in numerous zero values.

# 1-7

# Calculate word frequencies and extract the top 10 most frequent words
# The rowSums function calculates the frequency of each word, sorts them in descending order, and extracts the top 10 words.
term_freq <- sort(rowSums(as.matrix(ct.tweet.dtm)), decreasing = TRUE)
top_word_indices <- names(term_freq)[1:10]
top_words <- colnames(ct.tweet.dtm)[as.numeric(top_word_indices)]
top_words

# Re-process the text to make the word list more accurate

# Apply additional cleaning steps: remove punctuation and extra whitespace
# tm_map is used to remove punctuation and unnecessary whitespace.
ct.tweet.cleaned <- tm_map(ct.tweet.cleaned, removePunctuation)              # Remove punctuation
ct.tweet.cleaned <- tm_map(ct.tweet.cleaned, stripWhitespace)                # Remove whitespace

# Recalculate word frequencies and display the top 10 words
term_freq <- sort(rowSums(as.matrix(ct.tweet.dtm)), decreasing = TRUE)
top_word_indices <- names(term_freq)[1:10]
top_words <- colnames(ct.tweet.dtm)[as.numeric(top_word_indices)]
top_words

# Observations:
# The top list contains meaningful words like 'reform', 'pay', 'nationwid'.
# However, incomplete stems such as 'primaria' and 'twelv' remain, indicating the need for further cleaning.
# For example, we might use lemmatization instead of stemming or filter out irrelevant terms.

# 1-8

# Install and load the 'lsa' package for similarity analysis
# install.packages("lsa")
library(lsa)

# Calculate cosine similarity between documents
# The cosine function calculates the cosine similarity between documents in the DTM. Cosine similarity measures the similarity of documents based on the angle between vectors.
cosine_similarity <- cosine(as.matrix(ct.tweet.dtm))

# Define a function to calculate Jaccard similarity
# Jaccard similarity measures similarity by using the intersection and union of two sets.
jaccard_similarity <- function(x, y) {
  intersect_len <- length(intersect(x, y))  # Calculate the size of the intersection
  union_len <- length(union(x, y))          # Calculate the size of the union
  return(intersect_len / union_len)          # Return Jaccard similarity
}

# Calculate Jaccard similarity between Clinton and Trump tweets
# Extract words from each tweet vector and calculate the Jaccard similarity between the two sets.
hc_terms <- unique(unlist(strsplit(hc.vec, " ")))
dt_terms <- unique(unlist(strsplit(dt.vec, " ")))
jaccard_sim <- jaccard_similarity(hc_terms, dt_terms)
jaccard_sim







#2-1

# Install necessary packages (if not installed)
if (!require("tm")) install.packages("tm")

# Load the package
library(tm)

# Set folder path (example path, change to actual path)
folder_path <- "C:/work/bbc_text_set"

# Load all .txt files into a Corpus object
text.set <- VCorpus(DirSource(folder_path, encoding = "UTF-8"), readerControl = list(language = "en"))

# Check the number of elements
length(text.set)  # Verify if it's 417


#2-2

library(dplyr)

# Extract titles
text.title <- data.frame(
  id = 1:length(text.set),
  title = sapply(text.set, function(x) {
    lines <- strsplit(as.character(x$content), "\n")[[1]]
    lines[1] # Use the first line as the title
  }),
  stringsAsFactors = FALSE,
  row.names = NULL # Do not set row names
)

# Check results
head(text.title)


# Based on the titles of these articles, it appears that the topic of 
#text.set is news articles, possibly focused on political, social, and economic issues.


#2-3

# Define text preprocessing function
clean_text <- function(corpus) {
  corpus <- tm_map(corpus, content_transformer(tolower))          # Convert to lowercase
  corpus <- tm_map(corpus, removePunctuation)                     # Remove punctuation
  corpus <- tm_map(corpus, removeNumbers)                         # Remove numbers
  corpus <- tm_map(corpus, removeWords, stopwords("en"))          # Remove stopwords
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "£", replacement = "")  # Remove special character £
  corpus <- tm_map(corpus, stripWhitespace)                       # Remove whitespace
  
  # Assume the first line as the title, remove empty lines, and trim leading/trailing whitespace
  corpus <- tm_map(corpus, content_transformer(function(text) {
    lines <- unlist(strsplit(text, "\n"))
    lines <- lines[-1]                # Remove the first line (title)
    lines <- lines[lines != ""]       # Remove empty lines
    lines <- trimws(lines)            # Trim leading and trailing whitespace for each line
    return(lines)                     # Keep list format
  }))
  
  return(corpus)
}

# Create a cleaned Corpus object
text.set.clean <- clean_text(text.set)

# Check results (first article)
text.set.clean[[1]]$content



#2-4

# Define function to find representative term in each article
find_representative_term <- function(doc, id) {
  # Calculate word frequencies
  dtm <- DocumentTermMatrix(VCorpus(VectorSource(doc)))
  term_freq <- colSums(as.matrix(dtm))
  
  # Find the most frequent word
  representative_term <- names(which.max(term_freq))
  max_value <- max(term_freq)
  
  return(data.frame(id = id, representative_term = representative_term, value = max_value))
}

# Find representative terms for each article and create a data frame
rep.df <- do.call(rbind, lapply(1:length(text.set.clean), function(i) {
  find_representative_term(text.set.clean[[i]]$content, i)
}))


# Check results
# Print data frame (without row index)
print(rep.df, row.names = FALSE)

# In rep.df, representative terms were extracted from each article by calculating word frequencies.
# When comparing rep.df with text.title, we see that some representative terms relate well to the titles:
# For id 1, the term "pay" matches the title "Labour plans maternity pay rise."
# For id 2, the term "information"partially aligns with "Watchdog probes e-mail deletions."
# For id 3, the term "women" corresponds well with "Hewitt decries 'career sexism,'" indicating a focus on women’s issues.
# Overall, while some representative terms match the titles closely, others may only partially reflect the main topics.


#2-5

# Install and load necessary packages
if (!require("proxy")) install.packages("proxy")
if (!require("cluster")) install.packages("cluster")

library(proxy)
library(cluster)

# 1. Create Document-Term Matrix
dtm <- DocumentTermMatrix(VCorpus(VectorSource(sapply(text.set.clean, function(x) paste(x$content, collapse = " ")))))

# 2. Measure similarity (using Euclidean distance)
dist_matrix <- dist(as.matrix(dtm), method = "euclidean")

# 3. Perform hierarchical clustering
text.set.cluster <- hclust(dist_matrix, method = "ward.D2")

# 4. Visualization - Create dendrogram
# Dendrogram visualization - Set the number of clusters
# Dendrogram visualization - Limit height
plot(text.set.cluster, labels = FALSE, main = "Hierarchical Clustering of Articles", xlab = "", sub = "", ylim = c(0, 100))

# Dendrogram visualization - Zoom in on lower section
plot(text.set.cluster, labels = FALSE, main = "Zoomed Hierarchical Clustering (Lower Section)", xlim = c(0, 10), ylim = c(0, 10))

# Clipped specific part due to large number of articles

# Cut at a specific height and select only 10 sub-groups
cut_height <- 50  # Cut at a specific height
sub_clusters <- cutree(text.set.cluster, h = cut_height)

# Select only 10 unique clusters from the lower section
unique_clusters <- unique(sub_clusters)[1:10]  # Select lower 10 clusters
selected_indices <- which(sub_clusters %in% unique_clusters)

# Create a dendrogram for the selected indices
selected_dend <- as.dendrogram(text.set.cluster)
selected_dend <- prune(selected_dend, leaves = setdiff(seq_along(sub_clusters), selected_indices))

# Dendrogram visualization - Zoomed in on specific height range
plot(selected_dend, main = "Zoomed Hierarchical Clustering (Lower Section)", xlim = c(0, 20))

# To verify that clustering was effective, we extract the titles and a portion of the content from documents 264 and 175 as examples
doc_ids <- c(264, 175)

# Extract titles
selected_titles <- text.title[text.title$id %in% doc_ids, ]

# Extract the first 5 lines of content from each document
selected_contents <- lapply(doc_ids, function(id) {
  list(
    id = id,
    title = selected_titles[selected_titles$id == id, "title"],
    content = text.set.clean[[id]]$content[1:5]  # Only get the first 5 lines
  )
})

# Display results
for (doc in selected_contents) {
  cat("Document ID:", doc$id, "\n")
  cat("Title:", doc$title, "\n")
  cat("Content:\n", paste(doc$content, collapse = "\n"), "\n\n")
}


# The reason these two documents (ID 264 and ID 175) are in the same cluster is due to their similar themes and content.
# Both focus on the Labour Party's economic policies and election strategies, with an emphasis on contrasting Labour with the Conservative Party.
# Each document encourages voters to support Labour by highlighting its economic vision and public service investments.
# This similarity in subject matter likely led to these documents being grouped together in the clustering results, indicating that the clustering process has effectively categorized documents with related topics.
