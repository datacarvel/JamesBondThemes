library(devtools)
# devtools::install_github('charlie86/spotifyr') # Installing it from CRAN or just install.packages won't work
library(spotifyr)
library(rlist)
library(tidyverse)

Sys.setenv(SPOTIFY_CLIENT_ID = 'XXXXXXXXXXXXXXXXXXX')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'XXXXXXXXXXXXXXXXXXX')

access_token <- get_spotify_access_token()

# My reference for a list of all main themes : https://www.radiotimes.com/news/film/2020-01-15/james-bond-every-theme-song/
# Checked against this list : https://en.wikipedia.org/wiki/List_of_James_Bond_films

# I'm not sure there's any way of making the next step more automatic - since it's just 25 songs it'll definitely be quicker if I just retrieve the songs' IDs manually myself than looking at the perfect automated way of doing it
# There are a few playlists done by users containing all main themes from 007 movies but they may miss a song or have a few extra ones so I'd rather make that list myself
# I'll retrieve the track ids in chronological order in the hope that it will show up the same in the data
# I also decided to not that the remastered versions whenever possible

AllSongsIDs <- "7ogsG1i9iqaJoflN3oC9pU,08rooRwpg79ExKPXrZadey,3tQCpnCvozv9iCZiyzIiDJ,0Cq5h2Xnn6EqA5dhDQUcss,7e3RGqZ2FkFmcInDdgfJze,4aVz0wlJLCsU92yN5vytyV,3Tp3nTswQmMzmQRqZu4iM2,5Go7a9wnApTQm2nYqKBdm0,5SehBzhVSGT9R8UcIAWsvT,49RUdNvwSiUTC8fBh4KKoC,0XCszZIzEncBpgAMkKoDB2,4jRJWlkCn3pAwezfqFODU3,0e6jrQ880TI8IjAexAmtb6,6I4snLrVOrJsLdd43isc27,4tWe3Fr8HrQPq2iFOpEGZs,2ytwDkF0RLOi6qL2uJ2cQG,528QhCT2v3HgD71RmrSUNW,3peBSjcjaouspgb68WK2sk,6uWKcqOTna9mMPgkRRcWNb,6KUQfbYcFIGZtyr0mdQuv4,4MR9iW77LJoPPDjwAYbIZZ,01bMpqmvH031R417l3AQTA,6VObnIkLVruX4UVyxWhlqm,1PWnAEQcbwQwK759otUbta,73SpzrcaHk0RQPFP73vqVR" 

ThemeTracks <- get_track_audio_features(AllSongsIDs, authorization = access_token) # This will give you the track feature without the names of the artists and the songs titles

TrackInfo1 <- get_tracks(AllSongsIDs, authorization = access_token) # This gives me names of the artists and song titles, so we'll combine this and that above with a inner_join() so we have one dataset, not 2
TrackInfo2 <- TrackInfo1$artists %>% # For some reason the artists names are within a deeper-buried list, so we treat it separately
  list.select(name) %>% # As in artists names
  list.rbind() %>% # No-messing-around way to unlist something
  as.data.frame() # SO CONVENIENT ! Two observations had 2 artists so was a list of 2 values within a cell, but this function here just comma-separates them as one entry! #AnticipatedProblemSolved
TrackInfo3 <- data.frame(id = TrackInfo1$id, TrackInfo2, TrackInfo1$name)

# Joining everything by the common "id" column which is the songs ids from Spotify which are found and common in both datasets

AllData <- inner_join(TrackInfo3, ThemeTracks, by = "id")

# Some tracks have extra info in their title like "Original motion soundtrack", so I just manually get rid of them by replacing them.

AllData$TrackInfo1.name[1] <- "James Bond Theme"
AllData$TrackInfo1.name[8] <- "Live And Let Die"
AllData$TrackInfo1.name[21] <- "You Know My Name"
AllData$TrackInfo1.name[24] <- "Writing's On The Wall"

# Now we need to put the dataset in a format that ggplot likes, with gather()

### BAR CHART

ggplot(AllData, aes(x = factor(TrackInfo1.name, levels = TrackInfo1.name), y = valence + energy + danceability)) +
  geom_bar(stat = "identity", fill = "#00ea95") +# Super important, it allow us to use the bar chart for something other than counting the observations
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

### DOTTED TIMELINE

ggplot(AllData, aes(x = factor(TrackInfo1.name, levels = TrackInfo1.name), group = 1)) +
  geom_path(aes(y = energy), color = "green", size = 2) +
  geom_path(aes(y = valence), color = "blue", size = 2) +
  geom_path(aes(y = danceability), color = "red", size = 2) +
  geom_point(shape = 21, color= "#69b3a2", fill="#69b3a2", size = 4) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

### GENIUS PART : GRABBING LYRICS

library(genius)

### Taking the only essential data for data and getting rid of the list format of the column "name"
### Note that I removed the very first James Bond Theme as it is instrumental

NameSong <- as.data.frame(AllData[2:25,2:3])
NameSong$name <- as.character(NameSong$name)
  
### So since there are just two problematic entries I'll correct them manually. Two cases where more than one artist is credited - Live And Let Die and Another Way To Die - I just looked up Genius how those two songs where credited exactly

NameSong$name[21] <- "Jack White"
NameSong$name[7] <- "Paul McCartney and Wings"

SongsTibble <- as_tibble(NameSong)
names(SongsTibble) <- c("artist", "title")

### Now there is some bug between my computer or R or the genius package with the genius website/api. Randomly, I'll either get all the lyrics, some of them or not, so a way to verify this is to look at the number of unique tracks I have in my dataset - it should be 24 (25 James Bond themes minus the first instrumental one)
### So my nasty workaround here is to make a tibble that repeats itself so R will try 8 times for each song, betting here that this will be enough tries, thanks to the laws of statistical probability
### I did report this issue to the package author but so far he was unable to reproduce my issue - so you may not need to do that at all
### I apologize for this as it is a lame thing to do - Skip to !! HELLO !! if you don't have this issue and all songs' lyrics come out fine

SongsTibble999 <- bind_rows(SongsTibble, SongsTibble, SongsTibble, SongsTibble, SongsTibble, SongsTibble, SongsTibble, SongsTibble)

### !! HELLO !!

Songs_Lyrics <- SongsTibble999 %>%
  add_genius(artist, title, type = "lyrics")

unique(Songs_Lyrics$track_title)

### And it works, I've got 24 unique track titles, but now other problems occurred : pretty sure there are duplicates now, and that the manually-set chronological is gone now
### Just selecting the unique rows seems to do the trick, big thanks to the column "line", repeated lines of lyrics won't be discarded !

Songs_Lyrics_Done <- unique(Songs_Lyrics) # This version with the base R package works

# Songs_Lyrics_Done2 <- Songs_Lyrics %>% ##### This would be the Tidyverse way to do it, but both methods give the same result
#  distinct

# Now the chronological order is lost but I believe it doesn't matter for now as we'll be able to reorder it later
# Renaming it so it's less of a hassle to type

AllLyrics <- Songs_Lyrics_Done

# Now the next step would be to prepare the lyrics for analysis. In most examples I've found all of a song's lyrics are within a single character string, but here our lyrics are divided by line, where each row is a lyric line that is also associated with its artist name and track title, among other things

library(tidytext)

### Removing stop-words (a database of them are included in the tidytext package, words like "I", "the", "and")

# I took this little chunk of code from https://www.rostrum.blog/2018/06/05/tid-ye-text/ 

songswords <- AllLyrics %>%
  unnest_tokens(word, lyric) %>%
  anti_join(tidytext::stop_words)

# The following comes from the Text Mining With R web book you can find here : https://www.tidytextmining.com/sentiment.html
# As the authors note, this is an analysis of unigrams, not bigrams, which means that each word are analyzed individually, not in pairs or in longer combinations, so keep this little caveat in mind

install.packages("textdata") # required for the afinn and nrc databases
library(textdata)

# I find the afinn lexicon to be the most appropriate at first since I find this is the most closely related to the datapoint of "valence" from Spotify : a numerical degree of positiveness/negativeness
# In it, words have a value that can go from -5 to 5, minus being negative, so -4 is more negative than -1 which is more negative than 3, and 4 would be more positive than 3
# Since Spotify's "Valence" metric goes from 0 to 1, I'll divide the afinn score by 5 since it goes from -5 to 0 or 0 to 5

afinn <- songswords %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(track_title) %>%
  summarize(sentiment = mean(value/5)) %>% # In some other published examples, people tended to add all the values with sum(), but I'd rather control for the length by making a mean()
  mutate(method = "AFINN")
afinn

# Now I'd like to put Spotify's Valence on the same type of scale (-1 to 1, the former being only negative, the latter only negative)

# AllData is All spotify data, including valence
# NameSong are the artist and song title only, non-list for artist names
# afinn contains sentiment scores with track title but no artist name

ValenceNormd <- AllData[2:25,] %>% # Short for "Valence normalized"
  select(title = TrackInfo1.name, valence) %>%
  mutate(NormdValence = (1 - valence)) # So the Spotify Valence's 0 to 1 scale becomes a -1 to 1 scale

afinn <- rename(afinn, title = track_title)

# Now, by attempting to join both tables, in the meantime songs titles from genius and those from spotify haven't got the exact same capitalisation (ex: "From Russia With Love" and "From Russia with Love"), so joining by value won't work as those two strings are different for R (joining will happen but some data will be lacking)
# So we'll simply lowercase everything

ValenceNormd$title <- tolower(ValenceNormd$title)
afinn$title <- tolower(afinn$title)

# But then again, now "writing’s on the wall" isn't the same as "writing's on the wall" ... notice the difference?
# Yup, the freaking apostrophe !!!

afinn$title <- str_replace(afinn$title, "’", "'")

# And now do we join both datasets

Val_Plus_Sentim <- left_join(ValenceNormd, afinn, by = "title")

Val_Plus_Sentim <- Val_Plus_Sentim %>%
  mutate(NormdSentiment = (1 - (sentiment + 1)/2)) %>%
  mutate(BadMoodIndex = (NormdValence + NormdSentiment)/2) %>%
  mutate(artist = SongsTibble$artist)

library(extrafont)
font_import()
loadfonts(device="win")
fonts()
fonts <- fonttable()

### General Bad Mood Index (all negative sentiments combined + valence) - AFINN

ggplot(Val_Plus_Sentim, aes(x = factor(title, levels = title), y = BadMoodIndex)) +
  geom_bar(stat = "identity", fill = "#ff00af") + # Super important, it allow us to use the bar chart for something other than counting the observations
  theme_minimal(
  ) +
  theme(
    text = element_text(family = "Myanmar Text"),
    axis.text.x = element_text(angle = 45, vjust = 1.1, hjust = 1, size = 10, family = "Myriad Pro", color = "black"),
    axis.text.y = element_text(family = "Oswald Regular", size = 15, color = "#0edda1"),
    axis.title = element_text(color = "#0018ff", face = "bold"),
    axis.title.y = element_text(vjust = 7),
    plot.background = element_rect(fill = "white"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm"),
    plot.title = element_text(color = "#0018ff", face = "bold"),
    plot.subtitle = element_text(color = "#0018ff"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    ) +
  ylim(0, 1) + 
  labs(
    x = "James Bond Themes - chronological",
    y = "Bad Mood Index",
    title = "Which James Bond theme has the most negative mood? Billie Eilish's",
    subtitle = "Comparing all themes' AI-attributed combined lyrical and musical negativity scores",
    caption = "Source : Spotify Web API, Genius.com"
  ) + 
  #scale_x_discrete(breaks = NULL) +
  geom_text(aes(label = artist, angle = 90, hjust = 1.1, fontface = "bold"), color = "white")

### Same GBMI chart but flipped

BadMood <- ggplot(Val_Plus_Sentim, aes(x = factor(title, levels = title), y = BadMoodIndex)) +
  geom_bar(stat = "identity", fill = "#0edda1") +# Super important, it allow us to use the bar chart for something other than counting the observations
  theme_minimal(
  ) +
  theme(
    text = element_text(family = "Myanmar Text"),
    axis.text.x = element_text(family = "Oswald Regular", size = 15, color = "#0edda1"),
    axis.text.y = element_text(vjust = 0.25, hjust = 1, size = 13, color = "black", face = "italic", margin = margin(0, -10, 0 ,0)),
    axis.title = element_text(color = "#0018ff", face = "bold"),
    axis.title.y = element_text(vjust = 7),
    plot.background = element_rect(fill = "white"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm"),
    plot.title = element_text(color = "#0018ff", face = "bold"),
    plot.subtitle = element_text(color = "#0018ff"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
  ) +
  ylim(0, 1) + 
  labs(
    x = "James Bond Themes - chronological",
    y = "Bad Mood Index",
    title = "Which James Bond theme has the most negative mood? Billie Eilish's",
    subtitle = "Comparing all themes' AI-attributed combined lyrical and musical negativity scores",
    caption = "Source : Spotify Web API, Genius.com"
  ) + 
  geom_text(aes(label = artist, hjust = 1.1, fontface = "bold"), color = "white") +
  coord_flip()

### Sentiments according to qualitative attributes - joy, anger, sadness, surprise (among others) - NRC database

sentimentsNRC <- get_sentiments("nrc")
unique(sentimentsNRC$sentiment)

nrc_attributed <- songswords %>%
  inner_join(sentimentsNRC, by = "word") %>%
  group_by(track_title) %>%
  summarise(
    sadnessN = sum(sentiment == "sadness"),  # Number of emotion-specific words
    joyN = sum(sentiment == "joy"),
    trustN = sum(sentiment == "trust"),
    angerN = sum(sentiment == "anger"),
    sadness = sum(sentiment == "sadness")/n(),  # Percentage of words
    joy = sum(sentiment == "joy")/n(),
    trust = sum(sentiment == "trust")/n(),
    anger = sum(sentiment == "anger")/n(),
    totalN = n(), # total of all words, stop words were already excluded above
    )

nrc_attributed$track_title <- tolower(nrc_attributed$track_title)
NameSong$TrackInfo1.name <- tolower(NameSong$TrackInfo1.name)
colnames(NameSong) <- c("name", "track_title")
nrc_attributed$track_title <- str_replace(nrc_attributed$track_title, "’", "'") # Again only because of Sam Smith's apostrophe in his song title
NameSong$track_title <- str_replace(NameSong$track_title, "’", "'") # And I'm too lazy to look which one was problematic so I give the same order to both

NRC_full <- left_join(NameSong, nrc_attributed, by = "track_title")


### Visualizing the NRC qualitative sentiments - vertical/mobile

ValenceNormd2 <- ValenceNormd
colnames(ValenceNormd2) <- c("track_title", "valence", "NormdValence")
Val_Plus_NRCsent <- left_join(ValenceNormd2, NRC_full, by = "track_title")

# Now originally I modified valence so 1 = very negative and 0 = very positive, but since we want both sadness and joy scales to be 1 = most sad/most joyful, then for the negative sentiment valence will have to be 1 = most negative, while for positive sentiment valence will have to be 1 = most positive
# For that, we need to get back the duration data from Spotify, as the formula is what follows : Lyrical Emotion Index = the square root of (number of emotion-type words (ex: "angry words" / song duration)

AllData2 <- select(AllData, name, TrackInfo1.name, duration_ms) # AllData as in All Spotify Data only
AllData2$track_title <- tolower(AllData$TrackInfo1.name)
SpotifyDuration <- select(AllData2, track_title, duration_ms)

library(scales)

# To calculate the LyricalDensity, I turn to what the author of the spotifyr package itself did : https://www.rcharlie.com/blog/fitter-happier/
# The difference is that for me, in my Sadness Index, higher number = saddest, while lower number = least sad
# Also, for the positive emotions, like Joy Index, higher number = most joyful, while lower number = least joyful
# So this also explains below why positive indices use (1 - valence) while the positive one use simply 'valence'
# And contrary to my previous Bad Mood Index, the data is in absolute term, all compared not to the highest and lowest number in the data series, but by the ultimate maximum of 0 and 100
# Here below, it's in relative term : a given figure is compared to our data series' highest value, not an imaginary maximum of 100 %

Val_Plus_NRCsent2 <- Val_Plus_NRCsent %>%
  left_join(SpotifyDuration, by = "track_title")

# So basically here what we do is combine...
  # Spotify's valence data (whether the music sounds negative or positive), and
  # The amount of angry words in relation to a song's total number of words and share of emotion-specific words that are anger, trust, joy and sadness

Val_Plus_NRCsent3 <- Val_Plus_NRCsent2 %>%
  mutate(LyricalDensity = totalN / duration_ms * 1000) %>%
  
  mutate(SadnessIndex = (1 - valence) + (sadness * (1 + LyricalDensity)) / 2) %>%
  mutate(SadnessIndex = scales::rescale(SadnessIndex, to = c(0, 100))) %>%
  
  mutate(JoyIndex = valence + (joy * (1 + LyricalDensity)) / 2) %>%
  mutate(JoyIndex = scales::rescale(JoyIndex, to = c(0, 100))) %>%

  mutate(AngerIndex = (1 - valence) + (anger * (1 + LyricalDensity)) / 2) %>%
  mutate(AngerIndex = scales::rescale(AngerIndex, to = c(0, 100))) %>%  
  
  mutate(TrustIndex = valence + (trust * (1 + LyricalDensity)) / 2) %>%
  mutate(TrustIndex = scales::rescale(TrustIndex, to = c(0, 100))) 
  

### NRC Qualitative Indices visualization, vertical/mobile

### Sadness

Sadness <- ggplot(Val_Plus_NRCsent3, aes(x = factor(track_title, levels = track_title), y = SadnessIndex)) +
  geom_bar(stat = "identity", fill = "#a200ff") + # Super important, it allow us to use the bar chart for something other than counting the observations
  theme_minimal(
  ) +
  theme(
    text = element_text(family = "Myanmar Text"),
    axis.text.x = element_text(family = "Oswald Regular", size = 15, color = "#0edda1"),
    axis.text.y = element_text(vjust = 0.25, hjust = 1, size = 10, color = "black", face = "italic", margin = margin(0, -10, 0 ,0)),
    axis.title = element_text(color = "#0018ff", face = "bold"),
    axis.title.y = element_text(vjust = 7),
    plot.background = element_rect(fill = "white"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm"),
    plot.title = element_text(color = "#0018ff", face = "bold"),
    plot.subtitle = element_text(color = "#0018ff"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
  ) +
  ylim(0, 100) + 
  labs(
    x = "James Bond Themes - chronological",
    title = "How positive or negative are all the James Bond Movie Themes?",
    subtitle = "Each theme song according to the Sadness Index",
    caption = "Source : Spotify Web API, Genius.com"
  ) + 
  geom_text(aes(label = name, hjust = 1.1, fontface = "bold"), color = "white") +
  coord_flip()

### Joy

  Joy <- ggplot(Val_Plus_NRCsent3, aes(x = factor(track_title, levels = track_title), y = JoyIndex)) +
  geom_bar(stat = "identity", fill = "#ff0072") + # Super important, it allow us to use the bar chart for something other than counting the observations
  theme_minimal(
  ) +
  theme(
    text = element_text(family = "Myanmar Text"),
    axis.text.x = element_text(family = "Oswald Regular", size = 15, color = "#0edda1"),
    axis.text.y = element_text(vjust = 0.25, hjust = 1, size = 10, color = "black", face = "italic", margin = margin(0, -10, 0 ,0)),
    axis.title = element_text(color = "#0018ff", face = "bold"),
    axis.title.y = element_text(vjust = 7),
    plot.background = element_rect(fill = "white"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm"),
    plot.title = element_text(color = "#0018ff", face = "bold"),
    plot.subtitle = element_text(color = "#0018ff"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
  ) +
  ylim(0, 100) + 
  labs(
    x = "James Bond Themes - chronological",
    title = "How positive or negative are all the James Bond Movie Themes?",
    subtitle = "Each theme song according to the Joy Index",
    caption = "Source : Spotify Web API, Genius.com"
  ) + 
  geom_text(aes(label = name, hjust = 1.1, fontface = "bold"), color = "white") +
  coord_flip()

### Anger

Anger <- ggplot(Val_Plus_NRCsent3, aes(x = factor(track_title, levels = track_title), y = AngerIndex)) +
  geom_bar(stat = "identity", fill = "#620000") + # Super important, it allow us to use the bar chart for something other than counting the observations
  theme_minimal(
  ) +
  theme(
    text = element_text(family = "Myanmar Text"),
    axis.text.x = element_text(family = "Oswald Regular", size = 15, color = "#0edda1"),
    axis.text.y = element_text(vjust = 0.25, hjust = 1, size = 10, color = "black", face = "italic", margin = margin(0, -10, 0 ,0)),
    axis.title = element_text(color = "#0018ff", face = "bold"),
    axis.title.y = element_text(vjust = 7),
    plot.background = element_rect(fill = "white"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm"),
    plot.title = element_text(color = "#0018ff", face = "bold"),
    plot.subtitle = element_text(color = "#0018ff"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
  ) +
  ylim(0, 100) + 
  labs(
    x = "James Bond Themes - chronological",
    title = "How positive or negative are all the James Bond Movie Themes?",
    subtitle = "Each theme song according to the Anger Index",
    caption = "Source : Spotify Web API, Genius.com"
  ) + 
  geom_text(aes(label = name, hjust = 1.1, fontface = "bold"), color = "white") +
  coord_flip()

### Trust

Trust <- ggplot(Val_Plus_NRCsent3, aes(x = factor(track_title, levels = track_title), y = TrustIndex)) +
  geom_bar(stat = "identity", fill = "#3bcd2f") + # Super important, it allow us to use the bar chart for something other than counting the observations
  theme_minimal(
  ) +
  theme(
    text = element_text(family = "Myanmar Text"),
    axis.text.x = element_text(family = "Oswald Regular", size = 15, color = "#0edda1"),
    axis.text.y = element_text(vjust = 0.25, hjust = 1, size = 10, color = "black", face = "italic", margin = margin(0, -10, 0 ,0)),
    axis.title = element_text(color = "#0018ff", face = "bold"),
    axis.title.y = element_text(vjust = 7),
    plot.background = element_rect(fill = "white"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm"),
    plot.title = element_text(color = "#0018ff", face = "bold"),
    plot.subtitle = element_text(color = "#0018ff"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
  ) +
  ylim(0, 100) + 
  labs(
    x = "James Bond Themes - chronological",
    title = "How positive or negative are all the James Bond Movie Themes?",
    subtitle = "Each theme song according to the Trust Index",
    caption = "Source : Spotify Web API, Genius.com"
  ) + 
  geom_text(aes(label = name, hjust = 1.1, fontface = "bold"), color = "white") +
  coord_flip()


