
# abnoRmal

##### an analysis of The Strokes’ discography using the `spotifyr` package

![](strokes_reptilia_narrow.png)

------------------------------------------------------------------------

### setup

``` r
library(tidyverse)
library(spotifyr)
library(DT)
library(ggridges)
options(scipen = 999)
knitr::opts_chunk$set(message = F, warning = F)
theme_set(theme_minimal())
```

### linking to Spotify API

``` r
# setting up my Spotify client ID & client secret
Sys.setenv(SPOTIFY_CLIENT_ID = "client ID here")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "client secret here")
access_token = get_spotify_access_token()
```

### getting artist information on The Strokes from `spotifyr`

``` r
# using spotifyr to get data on The Strokes
strokes = get_artist_audio_features("the strokes")

# confirming we have the correct albums
strokes |>
  count(album_name)
```

    ##                   album_name  n
    ## 1                     Angles 10
    ## 2           Comedown Machine 11
    ## 3 First Impressions Of Earth 28
    ## 4                 Is This It 33
    ## 5               Room On Fire 11
    ## 6           The New Abnormal  9

there are too many tracks for “Is This It” and “First Impressions Of
Earth” - let’s inspect that (although I wish they had 33 and 28 tracks)

### finding where we have duplicate tracks

``` r
strokes |>
  filter(album_name == "First Impressions Of Earth") |>
  count(track_name) |>
  head(5)
```

    ##         track_name n
    ## 1       15 Minutes 2
    ## 2  Ask Me Anything 2
    ## 3 Electricityscape 2
    ## 4      Evening Sun 2
    ## 5    Fear of Sleep 2

looks like everything is double counted, there seems to just be one
duplicate - let’s inspect what’s different by looking at the multiple
entries for the album’s first track, “You Only Live Once”

### getting information for the track “You Only Live Once”

``` r
strokes |>
  filter(track_name == "You Only Live Once") |>
  select(artist_name, album_release_date, track_name, album_name)
```

    ##   artist_name album_release_date         track_name                 album_name
    ## 1 The Strokes         2006-01-03 You Only Live Once First Impressions Of Earth
    ## 2 The Strokes         2006-01-03 You Only Live Once First Impressions Of Earth

we have different values for the following variables:

- `danceability` (0.631 v. 0.630)
- `energy` (0.905 v. 0.908)
- `loudness` (-2.44 v. -2.42)
- `speechiness` (0.0325 v. 0.0326)
- `acousticness` (0.0328 v. 0.0238)
- `instrumentalness` (0.528 v. 0.592)
- `liveness` (0.125 v. 0.116)
- `valence` (0.969 v. 0.968)
- `tempo` (120.520 v. 120.522)

so how do we decide which one we keep and which we omit?

looking at [this
forum](https://community.spotify.com/t5/iOS-iPhone-iPad/Duplicates-of-the-same-albums/td-p/4542505),
it looks like i would probably want to default to the most recent
release. however, as you can see below, the different versions of the
album in the data have the same release date.

### checking album release dates

``` r
strokes |>
  filter(album_name == "First Impressions Of Earth") |>
  count(album_release_date)
```

    ##   album_release_date  n
    ## 1         2006-01-03 28

upon further inspection, i found my answer in the `album_images` column.
i’ll put the images side-by-side below and we can pretty easily see
which one is the right album cover.

![](fioe%20side%20by%20side.png)

so now i’ll make sure to omit all instances of the first version of the
album using the `album_id` variable, which is unique for each of the two
versions.

### omitting duplicate album

``` r
strokes = strokes |>
  filter(album_id != "1HQ61my1h3VWp2EBWKlp0n")
```

and now we need to address the same issue for The Strokes’ first album,
“Is This It”.

### checking for duplicates for the album “Is This It”

``` r
strokes |>
  filter(album_name == "Is This It") |>
  count(track_name) |>
  head(5)
```

    ##        track_name n
    ## 1 Alone, Together 3
    ## 2    Barely Legal 3
    ## 3 Hard To Explain 3
    ## 4      Is This It 3
    ## 5       Last Nite 3

here we have three different versions of the album. again, taking a look
at the album images, i can tell which one is the version currently on
Spotify, so i will choose that one to keep.

### omitting duplicates for “Is This It”

``` r
strokes = strokes |>
  filter(album_id != "1BbxngE1wn7Lzantkvket2" & album_id != "2yNaksHgeMQM9Quse463b5")

strokes |>
  count(album_name)
```

    ##                   album_name  n
    ## 1                     Angles 10
    ## 2           Comedown Machine 11
    ## 3 First Impressions Of Earth 14
    ## 4                 Is This It 11
    ## 5               Room On Fire 11
    ## 6           The New Abnormal  9

now the data is free of duplicates and we can resume with our analysis.

### overview of data

``` r
strokes |>
  select(artist_name, album_name, track_name) |>
  glimpse()
```

    ## Rows: 66
    ## Columns: 3
    ## $ artist_name <chr> "The Strokes", "The Strokes", "The Strokes", "The Strokes"~
    ## $ album_name  <chr> "The New Abnormal", "The New Abnormal", "The New Abnormal"~
    ## $ track_name  <chr> "The Adults Are Talking", "Selfless", "Brooklyn Bridge To ~

### valence (measure of energy) by album

``` r
album_levels = c("Is This It", "Room On Fire", "First Impressions Of Earth",
                 "Angles", "Comedown Machine", "The New Abnormal")

strokes$album_name = factor(strokes$album_name, levels = album_levels)

strokes |>
  ggplot(aes(valence, fct_rev(album_name))) +
  geom_density_ridges(aes(fill = album_name), scale = 0.9, col = "transparent", alpha = 0.75) +
  labs(x = "valence", y = NULL) +
  theme(legend.position = "none",
        axis.text.x = element_blank())
```

![](strokes_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### function to scrape song lyrics

``` r
get_song_lyrics = function(song) {
  df = get_lyrics_search(artist_name = "The Strokes",
                         song_title = song)
  str = ""
  for (i in 1:nrow(df)) {
    str = paste(str, df$line[i])
  }
  return(trimws(str))
}
```

the above function was able to scrape the lyrics for all but just a few
tracks. for the remaining tracks, I just searched for its lyrics and
added them manually.

### loading in data with all lyrics

``` r
strokes = read_csv("strokes_all_lyrics.csv", col_types = cols())
```

### wordclouds for most common lyrics for different parts of speech

``` r
library(tidytext)
library(wordcloud)
library(wesanderson)
library(dplyr)
library(yarrr)

word_count = strokes |>
  unnest_tokens(word, lyrics) |>
  count(word, sort = T) |>
  mutate(word = reorder(word, n)) |>
  ungroup()

generate_wordcloud = function(part) {
  valid_words = parts_of_speech |>
    filter(str_detect(pos, part)) |>
    pull(word)
  
  fdata = word_count |>
    filter(word %in% valid_words)
  
  wordcloud(words = fdata$word, freq = fdata$n,
            max.words = 100, random.order = F,
            colors = c("#ABCCD4", "#E9B3FF", "#82AC7E"))
}

generate_wordcloud("Noun")
```

![](strokes_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
generate_wordcloud("Adjective")
```

![](strokes_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
generate_wordcloud("Adverb")
```

![](strokes_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->

``` r
generate_wordcloud("Verb")
```

![](strokes_files/figure-gfm/unnamed-chunk-12-4.png)<!-- -->

### pirate plot of track “sonic score” by album

``` r
pirateplot(valence + danceability + energy ~ album_release_year, strokes,
           pal = c("#FFF58F", "#C10000", "#000000", "#FF7DEF", "#FF5F5A", "#5AA0FF"),
           xlab = "album", ylab = "sonic score", main = "sonic score = valence + danceability + energy",
           theme = 0, point.o = 0.7, avg.line.o = 1, jitter.val = .05, 
           bty = "n", cex.axis = 0.6, xaxt = "n")

axis(1, cex.axis = 0.6, lwd = 0)
legend("topright", c("1: Is This It", "2: Room on Fire", "3: First Impressions of Earth",
                     "4: Angles", "5: Comedown Machine", "6: The New Abnormal"), bty = "n", cex = 0.6)
```

![](strokes_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

### getting sonic scores for all tracks

``` r
sonic_tracks = function(album) {
  return(strokes |>
           mutate(sonic_score = valence + danceability + energy) |>
           select(album_name, track_name, sonic_score) |>
           arrange(desc(sonic_score)) |>
           filter(album_name == album))
}

all_albums = c("Is This It", "Room On Fire", "First Impressions Of Earth",
               "Angles", "Comedown Machine", "The New Abnormal")

lapply(all_albums, sonic_tracks)
```

    ## [[1]]
    ## # A tibble: 11 x 3
    ##    album_name track_name          sonic_score
    ##    <chr>      <chr>                     <dbl>
    ##  1 Is This It Alone, Together            2.34
    ##  2 Is This It The Modern Age             2.31
    ##  3 Is This It Last Nite                  2.31
    ##  4 Is This It When It Started            2.28
    ##  5 Is This It Is This It                 2.20
    ##  6 Is This It Barely Legal               2.16
    ##  7 Is This It Take It Or Leave It        1.82
    ##  8 Is This It Trying Your Luck           1.81
    ##  9 Is This It Hard To Explain            1.74
    ## 10 Is This It Someday                    1.74
    ## 11 Is This It Soma                       1.69
    ## 
    ## [[2]]
    ## # A tibble: 11 x 3
    ##    album_name   track_name              sonic_score
    ##    <chr>        <chr>                         <dbl>
    ##  1 Room On Fire The Way It Is                  2.29
    ##  2 Room On Fire Automatic Stop                 2.12
    ##  3 Room On Fire You Talk Way Too Much          1.97
    ##  4 Room On Fire Meet Me in the Bathroom        1.96
    ##  5 Room On Fire 12:51                          1.93
    ##  6 Room On Fire Reptilia                       1.91
    ##  7 Room On Fire Under Control                  1.77
    ##  8 Room On Fire What Ever Happened?            1.73
    ##  9 Room On Fire The End Has No End             1.72
    ## 10 Room On Fire I Can't Win                    1.62
    ## 11 Room On Fire Between Love & Hate            1.60
    ## 
    ## [[3]]
    ## # A tibble: 14 x 3
    ##    album_name                 track_name         sonic_score
    ##    <chr>                      <chr>                    <dbl>
    ##  1 First Impressions Of Earth You Only Live Once        2.50
    ##  2 First Impressions Of Earth Killing Lies              2.24
    ##  3 First Impressions Of Earth Juicebox                  2.03
    ##  4 First Impressions Of Earth Red Light                 2.02
    ##  5 First Impressions Of Earth On the Other Side         1.99
    ##  6 First Impressions Of Earth Evening Sun               1.91
    ##  7 First Impressions Of Earth Electricityscape          1.85
    ##  8 First Impressions Of Earth Razorblade                1.81
    ##  9 First Impressions Of Earth Ize of the World          1.6 
    ## 10 First Impressions Of Earth Fear of Sleep             1.59
    ## 11 First Impressions Of Earth Heart In a Cage           1.41
    ## 12 First Impressions Of Earth Vision of Division        1.40
    ## 13 First Impressions Of Earth 15 Minutes                1.29
    ## 14 First Impressions Of Earth Ask Me Anything           1.14
    ## 
    ## [[4]]
    ## # A tibble: 10 x 3
    ##    album_name track_name                      sonic_score
    ##    <chr>      <chr>                                 <dbl>
    ##  1 Angles     Machu Picchu                           2.39
    ##  2 Angles     Taken for a Fool                       2.34
    ##  3 Angles     Gratisfaction                          2.14
    ##  4 Angles     Two Kinds of Happiness                 2.07
    ##  5 Angles     Games                                  1.80
    ##  6 Angles     Life Is Simple in the Moonlight        1.72
    ##  7 Angles     Under Cover of Darkness                1.71
    ##  8 Angles     You're So Right                        1.63
    ##  9 Angles     Metabolism                             1.61
    ## 10 Angles     Call Me Back                           1.22
    ## 
    ## [[5]]
    ## # A tibble: 11 x 3
    ##    album_name       track_name                  sonic_score
    ##    <chr>            <chr>                             <dbl>
    ##  1 Comedown Machine One Way Trigger                    2.42
    ##  2 Comedown Machine Tap Out                            2.11
    ##  3 Comedown Machine 50/50                              2.04
    ##  4 Comedown Machine Partners In Crime                  2.04
    ##  5 Comedown Machine Welcome To Japan                   2.02
    ##  6 Comedown Machine Slow Animals                       1.81
    ##  7 Comedown Machine Happy Ending                       1.71
    ##  8 Comedown Machine Chances                            1.66
    ##  9 Comedown Machine 80's Comedown Machine              1.37
    ## 10 Comedown Machine All The Time                       1.36
    ## 11 Comedown Machine Call It Fate, Call It Karma        1.14
    ## 
    ## [[6]]
    ## # A tibble: 9 x 3
    ##   album_name       track_name                    sonic_score
    ##   <chr>            <chr>                               <dbl>
    ## 1 The New Abnormal The Adults Are Talking               1.99
    ## 2 The New Abnormal Eternal Summer                       1.72
    ## 3 The New Abnormal Bad Decisions                        1.69
    ## 4 The New Abnormal Brooklyn Bridge To Chorus            1.67
    ## 5 The New Abnormal Why Are Sundays So Depressing        1.59
    ## 6 The New Abnormal Selfless                             1.31
    ## 7 The New Abnormal Not The Same Anymore                 1.25
    ## 8 The New Abnormal Ode To The Mets                      1.24
    ## 9 The New Abnormal At The Door                          1.14

### lexical diversity: ratio of unique words to total number of words in a track

``` r
words_by_album = strokes |>
  select(album_name, track_name, lyrics) |>
  unnest_tokens(words, lyrics) |>
  group_by(album_name, track_name) |>
  summarise(n = n(),
            .groups = "drop")

unique_words_album = strokes |>
  select(album_name, track_name, lyrics) |>
  unnest_tokens(words, lyrics) |>
  group_by(album_name, track_name) |>
  distinct(words) |>
  group_by(album_name, track_name) |>
  summarise(n = n(),
            .groups = "drop")

track_lex_diversity = words_by_album |>
  left_join(unique_words_album, by = c("album_name", "track_name")) |>
  rename(words = n.x, unique_words = n.y) |>
  mutate(lexical_diversity = round(unique_words / words, 3)) |>
  arrange(desc(lexical_diversity)) |>
  select(album_name, track_name, lexical_diversity)

ld_limit = 10

bind_rows(head(arrange(track_lex_diversity, desc(lexical_diversity)), ld_limit),
          tail(arrange(track_lex_diversity, desc(lexical_diversity)), ld_limit)) |>
  arrange(desc(lexical_diversity)) |>
  mutate(album_name = factor(album_name, levels = album_levels)) |>
  ggplot(aes(reorder(track_name, lexical_diversity), lexical_diversity)) +
  geom_col(aes(fill = album_name), width = 0.5, alpha = 0.75) +
  geom_text(aes(label = lexical_diversity), hjust = -0.5, size = 3) +
  geom_vline(xintercept = 10.5, linetype = "dotted") +
  coord_flip(ylim = c(0, 0.75)) +
  labs(x = "song title", y = "lexical diversity", fill = NULL,
       title = "ten most and least lexically diverse Strokes tracks") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](strokes_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

### using AFINN lexicon to get sentiment scores

``` r
# getting word values from AFINN
afinn = strokes |>
  unnest_tokens(word, lyrics) |>
  inner_join(get_sentiments("afinn")) |>
  select(track_name, album_name, word, value)

sample_n(afinn, 10)
```

    ## # A tibble: 10 x 4
    ##    track_name              album_name                 word    value
    ##    <chr>                   <chr>                      <chr>   <dbl>
    ##  1 Automatic Stop          Room On Fire               yeah        1
    ##  2 Killing Lies            First Impressions Of Earth killing    -3
    ##  3 Razorblade              First Impressions Of Earth no         -1
    ##  4 Razorblade              First Impressions Of Earth forget     -1
    ##  5 Slow Animals            Comedown Machine           wrong      -2
    ##  6 The Modern Age          Is This It                 please      1
    ##  7 Taken for a Fool        Angles                     like        2
    ##  8 Fear of Sleep           First Impressions Of Earth fear       -2
    ##  9 Meet Me in the Bathroom Room On Fire               dead       -3
    ## 10 When It Started         Is This It                 dear        2

### which albums are the most positive or negative as a whole?

``` r
afinn |>
  group_by(album_name) |>
  summarise(value = sum(value)) |>
  mutate(album_name = factor(album_name, rev(album_levels))) |>
  ggplot(aes(album_name, value)) +
  geom_col(aes(fill = album_name)) +
  coord_flip() +
  labs(x = NULL, y = "negativity / positivity",
       title = "total lyrical sentiment by album") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))
```

![](strokes_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

### total positivity and negativity in each album

``` r
afinn |>
  mutate(pos_neg = ifelse(value > 0, "pos", "neg")) |>
  group_by(album_name, pos_neg) |>
  summarise(score = sum(value),
            .groups = "drop") |>
  mutate(album = paste0(album_name, "_", pos_neg),
         album_name = factor(album_name, levels = rev(album_levels)),
         pos_label = ifelse(score > 0, score, ""),
         neg_label = ifelse(score < 0, score, "")) |>
  ggplot(aes(album_name, score)) +
  geom_col(aes(fill = album)) +
  geom_text(aes(label = pos_label), size = 3, hjust = 1.5) +
  geom_text(aes(label = neg_label), size = 3, hjust = -0.5) +
  scale_fill_manual(values = c("#688BC4", "#C8D7F0", "#B071CE", "#E2C4F0", "#73B15B", "#AED59F",
                               "#D26969", "#F2B9B9", "#E5A83E", "#EED09D", "#D374C1", "#F7D7F1")) +
  coord_flip() +
  labs(x = NULL, y = "negativity / positivity",
       title = "positive and negative lyrical sentiment by album") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank())
```

![](strokes_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->
