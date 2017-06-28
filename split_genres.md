Creación de conjunto de datos para mostrar la puntuación por género
-------------------------------------------------------------------

Para la realización de este proyecto se ha utilizado el conjunto de datos de películas disponible en la siguiente página web:

<https://www.kaggle.com/deepmatrix/imdb-5000-movie-dataset>

Los datos se cargarán desde el directorio actual, almacenados en un fichero CSV:

``` r
fileMovies <- file("./movie_metadata_original.csv","r") 
moviesDataset <- read.csv(fileMovies) 
close(fileMovies) 
head(moviesDataset[,1:5])
```

Dividimos la columna con el genero para poder representarlo en Tableau:

``` r
if(! "splitstackshape" %in% installed.packages())
  install.packages("splitstackshape", repos="http://cran.rstudio.com/", depend = TRUE)
library(splitstackshape)
```

    ## Loading required package: data.table

``` r
moviesDatasetSplit <- concat.split.multiple(moviesDataset, "genres", direction = "long", sep = "|")
```

    ## This function is deprecated. Use `cSplit` instead.

``` r
head(moviesDatasetSplit[,1:10])
```

    ##    color  director_name num_critic_for_reviews duration
    ## 1: Color  James Cameron                    723      178
    ## 2: Color  James Cameron                    723      178
    ## 3: Color  James Cameron                    723      178
    ## 4: Color  James Cameron                    723      178
    ## 5: Color Gore Verbinski                    302      169
    ## 6: Color Gore Verbinski                    302      169
    ##    director_facebook_likes actor_3_facebook_likes     actor_2_name
    ## 1:                       0                    855 Joel David Moore
    ## 2:                       0                    855 Joel David Moore
    ## 3:                       0                    855 Joel David Moore
    ## 4:                       0                    855 Joel David Moore
    ## 5:                     563                   1000    Orlando Bloom
    ## 6:                     563                   1000    Orlando Bloom
    ##    actor_1_facebook_likes     gross    genres
    ## 1:                   1000 760505847    Action
    ## 2:                   1000 760505847 Adventure
    ## 3:                   1000 760505847   Fantasy
    ## 4:                   1000 760505847    Sci-Fi
    ## 5:                  40000 309404152    Action
    ## 6:                  40000 309404152 Adventure

Guardamos los datos procesados en un nuevo fichero para Tableau:

``` r
write.csv(moviesDatasetSplit, "./movie_metadata_genres.csv") 
```
