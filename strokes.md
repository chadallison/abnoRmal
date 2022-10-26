
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

-   `danceability` (0.631 v. 0.630)
-   `energy` (0.905 v. 0.908)
-   `loudness` (-2.44 v. -2.42)
-   `speechiness` (0.0325 v. 0.0326)
-   `acousticness` (0.0328 v. 0.0238)
-   `instrumentalness` (0.528 v. 0.592)
-   `liveness` (0.125 v. 0.116)
-   `valence` (0.969 v. 0.968)
-   `tempo` (120.520 v. 120.522)

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
