# Wczytaj dane tekstowe
# Wczytaj plik tekstowy z lokalnego dysku
text <- readLines(file.choose())
text

library(qdap)
library(wordcloud)

frequent_terms <- freq_terms(text)
frequent_terms
frequent_terms <- freq_terms(text, stopwords = Top200Words)
plot(frequent_terms)
my_stopwords <- c(Top200Words, "im", "ive", "thats", "thats")
frequent_terms <- freq_terms(text, stopwords = my_stopwords)
# Utwórz chmurę słów
wordcloud(frequent_terms$WORD, frequent_terms$FREQ)
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4)
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, max.words = 5)
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(9,"Reds"))
library(ggplot2)

ggplot(frequent_terms, aes(x = WORD, y = FREQ)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Słowo", y = "Częstość") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Wykres częstości słów")

ggplot(frequent_terms, aes(y = WORD, x = FREQ)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Słowo", y = "Częstość") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  ggtitle("Wykres częstości słów")

# Bardziej atrakcyjna wizualizacja
ggplot(frequent_terms, aes(x = FREQ, y = reorder(WORD, FREQ))) +
  geom_bar(stat = "identity", fill = "skyblue", color = "darkblue", alpha = 0.8) +
  labs(x = "Częstość", y = "Słowo") +
  ggtitle("Wykres częstości słów") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10), # Dostosowanie rozmiaru czcionki etykiet na osi Y
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Wyśrodkowanie i stylizacja tytułu wykresu
        panel.grid.major.y = element_blank(), # Usunięcie głównych linii siatki poziomej
        panel.grid.minor.y = element_blank(), # Usunięcie mniejszych linii siatki poziomej
        axis.line = element_line(color = "black")) # Dostosowanie linii osi