
# Wczytaj dane tekstowe
# Wczytaj plik tekstowy z lokalnego dysku
text <- readLines(file.choose())
text

text2024 <- readLines(file.choose())
text2024


install.packages("qdap")
library(qdap)

frequent_terms <- freq_terms(text)
frequent_terms
frequent_terms <- freq_terms(text, stopwords = Top200Words)
plot(frequent_terms)

frequent_terms2024 <- freq_terms(text2024)
frequent_terms2024
frequent_terms2024 <- freq_terms(text2024, stopwords = Top200Words)
plot(frequent_terms2024)


# Tworzenie chmury słów za pomocą pakietu wordcloud
install.packages("wordcloud")
library(wordcloud)



# Utwórz chmurę słów
wordcloud(frequent_terms$WORD, frequent_terms$FREQ)
wordcloud(frequent_terms2024$WORD, frequent_terms2024$FREQ)


# Opcje chmury słów
?wordcloud
# Zmiana wartości min.freq i max.words w celu wyświetlenia mniejszej/większej liczby słów.
# min.freq: słowa o częstości poniżej tej wartości nie będą wyświetlane
# max.words: maksymalna liczba słów do wyświetlenia


# Dodanie różnych palet kolorystycznych
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(9,"Blues"))
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(9,"Reds"))
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(9,"Greens"))

# Ograniczenie liczby słów w chmurze poprzez określenie minimalnej częstości
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4)
wordcloud(frequent_terms2024$WORD, frequent_terms2024$FREQ, min.freq = 4)

# Ograniczenie liczby słów w chmurze poprzez określenie maksymalnej liczby słów
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, max.words = 5)
wordcloud(frequent_terms2024$WORD, frequent_terms2024$FREQ, max.words = 5)

# Optymalizacja i dostosowanie wyników
# Dodanie koloru do chmury słów dla lepszej wizualizacji
# Dodanie koloru
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(8,"Dark2"))
wordcloud(frequent_terms2024$WORD, frequent_terms2024$FREQ, min.freq = 4, colors = brewer.pal(8,"Dark2"))


# Dodanie koloru
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, max.words = 5, colors = brewer.pal(8,"Accent"))
wordcloud(frequent_terms2024$WORD, frequent_terms2024$FREQ, max.words = 5, colors = brewer.pal(8,"Accent"))


?brewer.pal
brewer.pal.info
text

# najczęstsze słowa w 2021: 
# 1.American 
# 2.Jobs
# 3.Plan
# 4.Americans
# 5.Act
# 6.Families 
# 
# najczęstsze słowa w 2024:
# 1. President
# 2. American
# 3. Future
# 4. Years
# 5. Pay
# 6. Americans
