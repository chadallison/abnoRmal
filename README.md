
# abnoRmal

##### an analysis of The Strokes’ discography using the `spotifyr` package and my Spotify listening history

![](strokes_reptilia_narrow.png)

------------------------------------------------------------------------

``` r
# setting up my Spotify client ID & client secret
Sys.setenv(SPOTIFY_CLIENT_ID = "client ID here")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "client secret here")
access_token = get_spotify_access_token()
```

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

There are too many tracks for *Is This It* and *First Impressions Of
Earth* - let’s inspect that (although I wish they had 33 and 28 tracks)

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
entries for the album’s first track, *You Only Live Once*

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

``` r
strokes = strokes |>
  filter(album_id != "1HQ61my1h3VWp2EBWKlp0n")
```

and now we need to address the same issue for The Strokes’ first album,
*Is This It*.

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
spotify, so i will choose that one to keep.

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

``` r
strokes |>
  select(artist_name, album_name, track_name) |>
  head(9)
```

    ##   artist_name       album_name                    track_name
    ## 1 The Strokes The New Abnormal        The Adults Are Talking
    ## 2 The Strokes The New Abnormal                      Selfless
    ## 3 The Strokes The New Abnormal     Brooklyn Bridge To Chorus
    ## 4 The Strokes The New Abnormal                 Bad Decisions
    ## 5 The Strokes The New Abnormal                Eternal Summer
    ## 6 The Strokes The New Abnormal                   At The Door
    ## 7 The Strokes The New Abnormal Why Are Sundays So Depressing
    ## 8 The Strokes The New Abnormal          Not The Same Anymore
    ## 9 The Strokes The New Abnormal               Ode To The Mets

``` r
# genius_get_artists <- function(artist_name = "the strokes", n_results = 10) {
#   baseURL <- 'https://api.genius.com/search?q='
#   requestURL <- paste0(baseURL, gsub(' ', '%20', artist_name),
#                        '&per_page=', n_results,
#                        '&access_token=', access_token)
#   
#   res <- GET(requestURL) %>% content %>% .$response %>% .$hits
#   
#   map_df(1:length(res), function(x) {
#     tmp <- res[[x]]$result$primary_artist
#     list(
#       artist_id = tmp$id,
#       artist_name = tmp$name
#     )
#   }) %>% unique
# }
# 
# genius_artists = genius_get_artists("the strokes")
```

``` r
# names(strokes)[9:19]
album_levels = c("Is This It", "Room On Fire", "First Impressions Of Earth",
                 "Angles", "Comedown Machine", "The New Abnormal")

strokes$album_name = factor(strokes$album_name, levels = album_levels)

valence_by_album_plot = strokes |>
  ggplot(aes(valence, fct_rev(album_name))) +
  geom_density_ridges(aes(fill = album_name), scale = 0.9, col = "transparent", alpha = 0.75) +
  labs(x = "Valence", y = NULL) +
  theme(legend.position = "none",
        axis.text.x = element_blank())

valence_by_album_plot
```

![](strokes_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
strokes |>
  group_by(album_name) |>
  summarise(average_valence = round(mean(valence), 3)) |>
  arrange(desc(average_valence))
```

    ## # A tibble: 6 × 2
    ##   album_name                 average_valence
    ##   <fct>                                <dbl>
    ## 1 Is This It                           0.743
    ## 2 Room On Fire                         0.647
    ## 3 Angles                               0.563
    ## 4 First Impressions Of Earth           0.515
    ## 5 Comedown Machine                     0.501
    ## 6 The New Abnormal                     0.341

as the above ridgeline plot suggests, the valence of Strokes albums has
become progressively lower over the course of their discography, with
each successive album having a lower average valence than its
predecessor. next, let’s look at which songs have the highest and lowest
valence values.

``` r
# highest valence
strokes |>
  select(track_name, valence) |>
  mutate(valence = round(valence, 3)) |>
  arrange(desc(valence)) |>
  head(5)
```

    ##           track_name valence
    ## 1 You Only Live Once   0.969
    ## 2    One Way Trigger   0.964
    ## 3    Alone, Together   0.961
    ## 4       Killing Lies   0.927
    ## 5     Automatic Stop   0.914

``` r
# lowest valence
strokes |>
  select(track_name, valence) |>
  mutate(valence = round(valence, 3)) |>
  arrange(valence) |>
  head(5)
```

    ##           track_name valence
    ## 1       All The Time   0.078
    ## 2           Selfless   0.089
    ## 3    Heart In a Cage   0.121
    ## 4 Vision of Division   0.157
    ## 5       Call Me Back   0.185

### WORK ON GETTING THIS INTO A DATA FRAME

``` r
# how about the highest and lowest valence songs on each album
strokes$valence = round(strokes$valence, 3)

# strokes |>
#   mutate(valence = round(valence, 3)) |>
#   group_by(album_name) |>
#   summarise(min_valence = min(valence),
#             max_valence = max(valence))

"--- least valent tracks ---"
```

    ## [1] "--- least valent tracks ---"

``` r
paste("Is This It:", strokes$track_name[which(strokes$valence == 0.481)])
```

    ## [1] "Is This It: Take It Or Leave It"

``` r
paste("Room On Fire", strokes$track_name[which(strokes$valence == 0.403)])
```

    ## [1] "Room On Fire I Can't Win"

``` r
paste("First Impressions Of Earth:", strokes$track_name[which(strokes$valence == 0.121)])
```

    ## [1] "First Impressions Of Earth: Heart In a Cage"

``` r
paste("Angles:", strokes$track_name[which(strokes$valence == 0.185)])
```

    ## [1] "Angles: Call Me Back"

``` r
paste("Comedown Machine:", strokes$track_name[which(strokes$valence == 0.078)])
```

    ## [1] "Comedown Machine: All The Time"

``` r
paste("The New Abnormal:", strokes$track_name[which(strokes$valence == 0.089)])
```

    ## [1] "The New Abnormal: Selfless"

``` r
"--- most valent tracks ---"
```

    ## [1] "--- most valent tracks ---"

``` r
paste("Is This It:", strokes$track_name[which(strokes$valence == 0.961)])
```

    ## [1] "Is This It: Alone, Together"

``` r
paste("Room On Fire:", strokes$track_name[which(strokes$valence == 0.914)])
```

    ## [1] "Room On Fire: Automatic Stop"

``` r
paste("First Impressions Of Earth:", strokes$track_name[which(strokes$valence == 0.969)])
```

    ## [1] "First Impressions Of Earth: You Only Live Once"

``` r
paste("Angles:", strokes$track_name[which(strokes$valence == 0.885)])
```

    ## [1] "Angles: Taken for a Fool"

``` r
paste("Comedown Machine:", strokes$track_name[which(strokes$valence == 0.964)])
```

    ## [1] "Comedown Machine: One Way Trigger"

``` r
paste("The New Abnormal:", strokes$track_name[which(strokes$valence == 0.649)])
```

    ## [1] "The New Abnormal: The Adults Are Talking"

``` r
# library(geniusr)
# genius_token(T)
# client id = u2X4arrcpERbK2qUOuWi4-SNWDNUJ8bMinDq6jbQPFWKFthB-g5mHNOZjer1Hk7z
# client secret = kOLR9vdB1UsVAOT8gttk2EyeMULG71AL4tVuGQlSpvIiy0YNYo8J_NwZerYm-0FATalY4ydA7SRJnTzHS4zfBA
# access token = q2nsw8oL2l8J5Qs6-Y2DbczMuEUBVgyZ5GetLPuDdxbia3kP78xssEwKSt4GXif0
```

``` r
# library(geniusr)
# library(dplyr)
# library(tidytext)

# get lyrics
# get_lyrics_search(artist_name = "Kanye West",
#                   song_title = "Good Morning") %>% 
#   # get lyric bigrams
#   unnest_tokens(bigram, line, token = "ngrams", n = 2) %>%
#   # look for good morning
#   filter(bigram == "good morning") %>% 
#   # count bigram frequency
#   nrow()

# songs_df = data.frame(song = c("Soma", "Is This It"), lyrics = NA)
# for (i in 1:nrow(songs_df)) {
#   df1 = get_lyrics_search(artist_name = "The Strokes", song_title = songs_df$song[1])
#   str = ""
#   for (i in 1:nrow(df1)) { str = paste(str, df1$line[i]) }
#   lyrics = trimws(str)
#   songs_df$lyrics[i] = lyrics
#   songs_df
# }

### THE ABOVE ISN'T WORKING - MIGHT HAVE TO TRY A JOIN INSTEAD ###

# get_lyrics_search(artist_name = "The Strokes",
#                   song_title = "Soma")

# get_artist_songs(artist_id = "The Strokes")
```

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

``` r
# songs = c("Soma", "Someday")
#
# for (song in songs) {
#   print(get_song_lyrics("The Strokes", song))
# }
```

``` r
# strokes$lyrics = "nothing yet"
```

``` r
# num = 1
# song_name = strokes$track_name[num]
# lyrics = get_lyrics_search("The Strokes", song_name) |>
#   pull(line)
# 
# str = ""
# 
# for (i in 1:length(lyrics)) {
#   str = paste(str, lyrics[i])
# }
# 
# str = trimws(str)
# song_df = data.frame(track_name = song_name, lyrics = str)
# 
# strokes = left_join(strokes, song_df, by = "track_name")
```

``` r
# lyrs = read_csv("all_with_lyrics.csv", col_types = cols())
# strokes = left_join(strokes, lyrs, by = "track_name")
```

``` r
### this works, chunk below will try to do it in a loop ###
# strokes |>
#   arrange(album_release_date) |>
#   select(track_name, lyrics)
# 
# track = "50/50"
# lyr = get_song_lyrics(track)
# index = which(strokes$track_name == track)
# strokes$lyrics[index] = lyr

# strokes[which(is.na(strokes$lyrics)), ] |>
#   select(track_name, lyrics)
```

``` r
# strokes$lyrics[which(strokes$track_name == "50/50")] = "Why's she telling me the story of her life? All the things you wanna kill will give you spite And if you've taken all the prisoners inside As they're doling out their wisdom in the fire I will say! I will say don't judge me! I will say! I will say don't judge me! I wait on a darkened highway! I wait on a darkened highway! I can take as long as without looking by Why's she telling me the story of her life And if you've taken all the prisoners inside As they've thrown all their wisdom in the fire I will say! I will say don't judge me! I will say! I will say don't judge me! I wait on a darkened highway! I wait on a darkened highway! I will say! I will say don't judge me! I will say! I will say don't judge me! I wait on a darkened highway! I wait on a darkened highway!"
# 
# strokes$lyrics[which(strokes$track_name == "12:51")] = "Talk to me now I'm older Your friend told you 'cause I told her Friday nights have been lonely Change your plans and then phone me We could go and get forties Fuck goin' to that party Oh, really, your folks are away now? Alright, let's go, you convinced me 12:51 is the time my voice Found the words I sought Is it this stage I want? The world is shutting out for us Oh, we were tense for sure But we was confident Kiss me now that I'm older I won't try to control you Friday nights have been lonely Take it slow but don't warn me We'd go out and get forties Then we'd go to some party Oh, really, your folks are away now? Alright, I'm coming I'll be right there"
```

``` r
# strokes |>
#   count(lyrics)
```

# WE FINALLY HAVE ALL THE LYRICS

``` r
strokes = read_csv("strokes_all_lyrics.csv", col_types = cols())
```

``` r
library(tidytext)
library(wordcloud)
library(wesanderson)

word_count = strokes |>
  unnest_tokens(word, lyrics) |>
  count(word, sort = T) |>
  mutate(word = reorder(word, n)) |>
  ungroup()

omit_words = c("i", "you", "the", "to", "it", "a", "me", "and", "oh", "your", "in",
               "on", "that", "for", "is", "but", "i'm", "no", "not", "you're", "of",
               "we", "my", "so", "it's", "be", "they", "was", "yeah")

word_count = word_count |> filter(!word %in% omit_words)

wordcloud(words = word_count$word, freq = word_count$n,
          max.words = 100, random.order = F,
          colors = c("#ABCCD4", "#E9B3FF", "#82AC7E"))
```

![](strokes_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->
