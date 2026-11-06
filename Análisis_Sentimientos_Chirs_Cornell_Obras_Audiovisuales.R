# ================================================================
# SCRIPT: Sentiment Analysis of Chris Cornell's Music Videos (≤ 2017)
#          with Separate Word‑Frequency Plots (Top 18, 20, 25)
# LANGUAGE: American English
# ================================================================

rm(list = ls(all = TRUE))

# 1. Install and load required libraries
# --------------------------------------
packages <- c("tidyverse", "tidytext", "wordcloud", "RColorBrewer", 
              "textdata", "ggplot2", "stopwords")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# 2. Load the previously saved database
# -------------------------------------
cache_file <- "letras_videoclips_lrclib.rds"
csv_file <- "letras_videoclips_lrclib.csv"

if (file.exists(cache_file)) {
  lyrics_df <- readRDS(cache_file)
  message("Data loaded from: ", cache_file)
} else if (file.exists(csv_file)) {
  lyrics_df <- read_csv(csv_file, col_types = cols())
  message("Data loaded from: ", csv_file)
} else {
  stop("No data file found. Please run the download script first.")
}

# 3. Filter to releases up to 2017 (exclude posthumous 2020 tracks)
# -----------------------------------------------------------------
lyrics_df <- lyrics_df %>% filter(year <= 2017)
cat("\n=== DATA OVERVIEW ===\n")
cat("Songs with lyrics (≤ 2017):", nrow(lyrics_df), "\n")
cat("Artists:", paste(unique(lyrics_df$artist), collapse = ", "), "\n")
cat("Years covered:", paste(sort(unique(lyrics_df$year)), collapse = ", "), "\n\n")

# 4. Tokenization (US English stopwords)
# --------------------------------------
tidy_lyrics <- lyrics_df %>%
  unnest_tokens(word, lyrics) %>%
  filter(!str_detect(word, "^[0-9]+$")) %>%
  anti_join(get_stopwords(language = "en"), by = "word")

cat("Unique words (lexicon size):", length(unique(tidy_lyrics$word)), "\n\n")

# 5. FREQUENCY PLOTS: Top 18, 20, and 25 words (SEPARATE)
# -------------------------------------------------------
word_counts <- tidy_lyrics %>%
  count(word, sort = TRUE)

plot_top_n <- function(n) {
  word_counts %>%
    slice_max(n, n = n) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(x = n, y = word, fill = n)) +
    geom_col(show.legend = FALSE) +
    scale_fill_gradient(low = "skyblue", high = "navy") +
    labs(title = paste("Top", n, "Most Frequent Words in Music Videos (≤ 2017)"),
         subtitle = "Chris Cornell, Soundgarden, Audioslave, Temple of the Dog",
         x = "Frequency", y = NULL) +
    theme_minimal()
}

p18 <- plot_top_n(18)
p20 <- plot_top_n(20)
p25 <- plot_top_n(25)

print(p18)
print(p20)
print(p25)

ggsave("top18_words.png", p18, width = 8, height = 6)
ggsave("top20_words.png", p20, width = 8, height = 6)
ggsave("top25_words.png", p25, width = 8, height = 6)
cat("✅ Frequency plots saved.\n")

# 6. SENTIMENT ANALYSIS PER SONG (Bing lexicon)
# ---------------------------------------------
sentiment_bing <- get_sentiments("bing")

# CORRECCIÓN DE SINTAXIS: Aseguramos que 'year' se mantenga en el pivot
sentiment_songs <- tidy_lyrics %>%
  inner_join(sentiment_bing, by = "word") %>%
  count(artist, track, year, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(total_words = positive + negative,
         sentiment_score = (positive - negative) / total_words) %>%
  mutate(sentiment_score = ifelse(total_words == 0, 0, sentiment_score))

# 7. PLOT: Sentiment per song (chronological)
# --------------------------------------------
p_sentiment_songs <- sentiment_songs %>%
  mutate(track_label = paste0(artist, " - ", track)) %>%
  ggplot(aes(x = reorder(track_label, year), y = sentiment_score, fill = sentiment_score > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = c("red3", "green3")) +
  labs(title = "Sentiment Score per Music Video (1989–2017)",
       subtitle = "Positive (green) vs. Negative (red) – Chronological order",
       x = NULL, y = "Sentiment Score (Pos - Neg) / Total sentiment words") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7))

# 8. PLOT: Average sentiment over years (trend)
# ---------------------------------------------
# CORRECCIÓN DE SINTAXIS: Aquí es donde fallaba el group_by(year)
sentiment_year <- sentiment_songs %>%
  group_by(year) %>%
  summarise(avg_sentiment = mean(sentiment_score, na.rm = TRUE)) %>%
  ungroup()

p_trend <- ggplot(sentiment_year, aes(x = year, y = avg_sentiment)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(size = 3, color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  labs(title = "Mood Trajectory: Average Sentiment by Year (≤ 2017)",
       subtitle = "Zero line = neutral. Grunge era shows negative dips; later periods shift",
       x = "Year", y = "Average Sentiment Score") +
  scale_x_continuous(breaks = seq(1989, 2017, by = 4)) +
  theme_minimal()

# 9. PLOT: Mood correlation across musical periods
# ------------------------------------------------
period_mapping <- lyrics_df %>%
  mutate(period = case_when(
    artist == "Temple of the Dog" ~ "Temple of the Dog (1991)",
    artist == "Audioslave" ~ "Audioslave (2002–2006)",
    artist == "Soundgarden" & year <= 1996 ~ "Soundgarden (1989–1996)",
    artist == "Soundgarden" & year > 1996 ~ "Soundgarden (2010)",
    artist == "Chris Cornell" ~ "Solo (1999–2015)",
    TRUE ~ "Other"
  )) %>%
  select(artist, track, year, period) %>%
  distinct()

sentiment_songs <- sentiment_songs %>%
  left_join(period_mapping, by = c("artist", "track", "year"))

sentiment_period <- sentiment_songs %>%
  group_by(period) %>%
  summarise(avg_sentiment = mean(sentiment_score, na.rm = TRUE),
            n_songs = n()) %>%
  ungroup() %>%
  filter(!is.na(period))

p_period <- ggplot(sentiment_period, aes(x = reorder(period, avg_sentiment), 
                                         y = avg_sentiment, fill = avg_sentiment > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = c("red3", "green3")) +
  labs(title = "Mood Correlation by Musical Era",
       subtitle = "Soundgarden (90s) darkest → Audioslave (positive) → Solo (reflective)",
       x = NULL, y = "Average Sentiment per Period") +
  theme_minimal()

# 10. Word Cloud (≤ 2017)
# -----------------------
set.seed(123)
word_freq <- tidy_lyrics %>%
  count(word) %>%
  filter(n > 5)

png("wordcloud_chris_cornell_2017.png", width = 800, height = 600, res = 120)
wordcloud(words = word_freq$word,
          freq = word_freq$n,
          max.words = 150,
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"),
          main = "Word Cloud of Chris Cornell's Lyrics (Music Videos ≤ 2017)")
dev.off()
cat("✅ Word cloud saved.\n")

# 11. Display all sentiment plots
# -------------------------------
print(p_sentiment_songs)
print(p_trend)
print(p_period)

# 12. Save sentiment results to CSV
# ---------------------------------
write_csv(sentiment_songs, "sentiment_per_song_upto2017.csv")
write_csv(sentiment_year, "sentiment_per_year_upto2017.csv")
cat("\n✅ Analysis completed. CSV outputs saved.\n")