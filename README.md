# James Bond Themes : a musical sentiment analysis

Which James Bond Theme Song has the baddest mood? I looked all the official tracks' Spotify audio features data and also carried a sentiment analysis of their lyrics.

# Here is for the overall negative mood :

![](https://github.com/datacarvel/JamesBondThemes/blob/main/BadMoodIndex-desktop2.png)

Excluding the very first theme track which is instrumental, I carried a sentiment analysis using Spotify’s audio features for each track (notably the “valence” metric, which measures how positive or negative a track sounds) combined with a text sentiment analysis thanks to Genius.com for the lyrics. The text analysis part was possible thanks to the AFINN lexicon published by Informatics and Mathematical Modelling, Technical University of Denmark, a database attributing absolute positivity or negativity to words.  

It was possible to calculate the Bad Mood Index by having both musical and lyrical data on the same scale (here 0 for positive, 1 for negative) and calculating the mean. 

# And here are mobile-friendly charts showing 4 specific sentiments 

## Joy

![](https://github.com/datacarvel/JamesBondThemes/blob/main/Joy.png)

## Sadness

![](https://github.com/datacarvel/JamesBondThemes/blob/main/Sadness.png)

## Trust

![](https://github.com/datacarvel/JamesBondThemes/blob/main/Trust.png)

## Anger

![](https://github.com/datacarvel/JamesBondThemes/blob/main/Anger.png)

# Thanks to everyone whose work helped me do this little project :

https://www.rcharlie.com/blog/fitter-happier/ (author of the spotifyr package's take on Radiohead songs)
#
Genius R package : https://github.com/JosiahParry/genius
#
https://towardsdatascience.com/angriest-death-grips-data-song-anger-code-through-r-ded3aa2fe844
#
https://www.tidytextmining.com/sentiment.html
#
https://medium.com/@simranvatsa5/taylor-f656e2a09cc3
#
https://developer.spotify.com/documentation/web-api/reference/tracks/get-audio-features/
#
https://github.com/wch/extrafont
#
https://paldhous.github.io/NICAR/2019/r-text-analysis.html
#
AFINN text sentiment database, Informatics and Mathematical Modelling, Technical University of Denmark : http://www2.imm.dtu.dk/pubdb/pubs/6010-full.html
#
National Research Council of Canada text sentiment database by Saif Mohammad, NRC Word-Emotion Association Lexicon (aka EmoLex) : http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
