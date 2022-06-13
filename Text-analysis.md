## Research Purpose

Online Job advertisement is a primary method for recruitment.
Researchers already apply text mining techniques to online job
advertisement to track job-related information, e.g., skills required
for different jobs. In online job advertisement, employers also post
crucial organization-related information for attracting applicants,
e.g., organizational culture, career development program. However,
research seldomly investigates how organization-related information
affects recruitment outcomes.

To fill this gap, the current project has two purposes: a) investigating
the relations between organizational cultures-Adaptability, Integrity,
Collaborative, Results-oriented, Customer-oriented, Detail-oriented from
the Organizational Culture Profile (OCP; Chatman et al. 2014)
framework-and organizational attractiveness in the online job
advertisement context by means of multilevel mixed-effects regression
analysis, b) developing deep learning modelling algorithms that can
predict the six organizational cultures and organizational
attractiveness from online job advertisement text separately.

## Organizational Culture

Definition: Things that are valued or rewarded within a specific
organization – that is, the pattern of beliefs and expectations shared
by members, and the behaviors that result from them.

## Organizational Culture Profile

Adaptability: being innovative, risk taking, being willing to
experiment, fast moving, being quick to take advantage of opportunities,
not being constrained by many rules

Integrity: having integrity, having high ethical standards, being
honest, respecting individuals, being fair

Collaborative: working in collaboration with others, being team
oriented, cooperative, being supportive, avoid conflict

Results oriented: being results oriented, having high expectations for
performance, achievement

Customer oriented: being customer oriented, listening to customers,
being market driven

Detail oriented: paying attention to detail, emphasizing quality, being
precise

The original format of the scale is Q-sort task, which is relatively
time- and energy-consuming for participants. Therefore, we decide to
develop and validate more easy question formats: Likert scale item and
point allocation task.

## A Pilot Study

313 participants and 60 job advertisements extracted from a professional
recruitment website. After reading organization-related information from
each job advertisement, participants rated the described organization on
the six OCP organizational cultures and organizational attractiveness.

We calculated the validation coefficients for the different question
formats.

Generally, the different question format converged well with each other.
Questions measuring the same culture dimension indicated high
correlations than questions measuring different culture dimensions.

## I am Personally not Satisfied With Current Outcome

We didnt’ replicate the six organizational cultures by using the OCP
following the original authors’ procedure.

Whether the dimensions people use to communicate organizational cultures
are different from the employers try to communicate in job
advertisements?

I think there it needs a more validation step based on a relative large
sample size having relatively good external validity. That is applying
text mining techniques to job advertisements.

## A New Validation Step by Using Structural Topic Modeling

Compare the culture keywords from the OCP with the topics returned by
the STM.

Because we have 60 descriptions rated by on average 10 raters, we can
make use of these scores to make further comparisons. For example, we
choose the description with high OCP Integrity score and see the
dominant topics that the STM characterize.

``` r
Organizaitonal_Culture <- readxl::read_xlsx("companies_2ndPartwith1ndPart60.xlsx",na = "",col_names = T, sheet = "Sheet2")
```

``` r
library(tm)
library(tidyverse)
library(tidytext)
library(dplyr)
```

``` r
# by using tm package, create corpus dataframe for text data 
Organizaitonal_culture_corpus <- Corpus(VectorSource(as.vector(Organizaitonal_Culture$company_description))) 
Organizaitonal_culture_corpus
```

    ## <<SimpleCorpus>>
    ## Metadata:  corpus specific: 1, document level (indexed): 0
    ## Content:  documents: 2333

``` r
# by using tidyverse package, create tidy-text data, punctuations are automatically removed and all words are automatically made lower case 
Organizaitonal_culture_tidytext <- Organizaitonal_Culture %>%
    select(company,company_description) %>%
    unnest_tokens("word", company_description)
head(Organizaitonal_culture_tidytext)
```

    ## # A tibble: 6 × 2
    ##   company                         word  
    ##   <chr>                           <chr> 
    ## 1 FFL Agent Force - Stokes Agency at    
    ## 2 FFL Agent Force - Stokes Agency ffl   
    ## 3 FFL Agent Force - Stokes Agency agent 
    ## 4 FFL Agent Force - Stokes Agency force 
    ## 5 FFL Agent Force - Stokes Agency stokes
    ## 6 FFL Agent Force - Stokes Agency agency

``` r
Organizaitonal_culture_tidytext %>%
  count(word) %>%
    arrange(desc(n))
```

    ## # A tibble: 28,126 × 2
    ##    word      n
    ##    <chr> <int>
    ##  1 and   24754
    ##  2 the   17779
    ##  3 to    15605
    ##  4 of    12648
    ##  5 our   10973
    ##  6 a     10216
    ##  7 in     9597
    ##  8 we     8144
    ##  9 is     5920
    ## 10 for    5721
    ## # … with 28,116 more rows

``` r
data("stop_words")
Organizaitonal_culture_tidytext <- Organizaitonal_culture_tidytext %>%
      anti_join(stop_words)
```

    ## Joining, by = "word"

``` r
Organizaitonal_culture_tidytext %>%
  count(word) %>%
    arrange(desc(n))
```

    ## # A tibble: 27,503 × 2
    ##    word          n
    ##    <chr>     <int>
    ##  1 services   2505
    ##  2 overview   2266
    ##  3 company    1737
    ##  4 care       1699
    ##  5 business   1533
    ##  6 team       1494
    ##  7 service    1480
    ##  8 employees  1479
    ##  9 clients    1420
    ## 10 benefits   1394
    ## # … with 27,493 more rows

``` r
# it is possible to remove more meaningless words by creating a custom list
Custom_removal <- t(as.data.frame(c("overview", "SMART")))  # 'overview' is the field title for all d
Custom_removal <- Custom_removal %>% as.data.frame(row.name = as.numeric("1")) %>% rename(word = V1, lexicon = V2)

Organizaitonal_culture_tidytext <- Organizaitonal_culture_tidytext %>%
      anti_join(Custom_removal)
```

    ## Joining, by = "word"

``` r
Organizaitonal_culture_tidytext %>%
  count(word) %>%
    arrange(desc(n))
```

    ## # A tibble: 27,502 × 2
    ##    word          n
    ##    <chr>     <int>
    ##  1 services   2505
    ##  2 company    1737
    ##  3 care       1699
    ##  4 business   1533
    ##  5 team       1494
    ##  6 service    1480
    ##  7 employees  1479
    ##  8 clients    1420
    ##  9 benefits   1394
    ## 10 people     1376
    ## # … with 27,492 more rows

``` r
str(Organizaitonal_culture_tidytext)
```

    ## tibble [289,576 × 2] (S3: tbl_df/tbl/data.frame)
    ##  $ company: chr [1:289576] "FFL Agent Force - Stokes Agency" "FFL Agent Force - Stokes Agency" "FFL Agent Force - Stokes Agency" "FFL Agent Force - Stokes Agency" ...
    ##  $ word   : chr [1:289576] "ffl" "agent" "force" "stokes" ...

``` r
Organizaitonal_culture_NoNumbers <- Organizaitonal_culture_tidytext[-grep("\\b\\d+\\b", Organizaitonal_culture_tidytext$word), ]

Organizaitonal_culture_NoNumbers %>%
  count(word) %>%
    arrange(desc(n))
```

    ## # A tibble: 25,079 × 2
    ##    word          n
    ##    <chr>     <int>
    ##  1 services   2505
    ##  2 company    1737
    ##  3 care       1699
    ##  4 business   1533
    ##  5 team       1494
    ##  6 service    1480
    ##  7 employees  1479
    ##  8 clients    1420
    ##  9 benefits   1394
    ## 10 people     1376
    ## # … with 25,069 more rows

``` r
# maybe this step is no longer necessary because I don't see there is a difference by running this line of code

Organizaitonal_culture_NoNumbers$word <- gsub("\\s+","",Organizaitonal_culture_NoNumbers$word)
```

``` r
library(SnowballC)
```

``` r
Organizaitonal_culture_NoNumbers <- Organizaitonal_culture_NoNumbers %>%
      mutate_at("word", list(~wordStem((.), language="en")))

Organizaitonal_culture_NoNumbers %>%
  count(word) %>%
    arrange(desc(n))
```

    ## # A tibble: 19,883 × 2
    ##    word         n
    ##    <chr>    <int>
    ##  1 servic    4031
    ##  2 provid    2567
    ##  3 compani   2562
    ##  4 employe   2063
    ##  5 opportun  1927
    ##  6 care      1870
    ##  7 busi      1837
    ##  8 custom    1836
    ##  9 client    1781
    ## 10 career    1705
    ## # … with 19,873 more rows

``` r
# Creat the document-term matrix for subsequent text analysis

Organizaitonal_culture_DTM <-
  Organizaitonal_culture_NoNumbers %>%
  count(company, word) %>%
  cast_dtm(company, word, n)
```

``` r
print(Organizaitonal_culture_DTM)
```

    ## <<DocumentTermMatrix (documents: 2259, terms: 19883)>>
    ## Non-/sparse entries: 191447/44724250
    ## Sparsity           : 100%
    ## Maximal term length: 64
    ## Weighting          : term frequency (tf)

``` r
library(topicmodels)
library(tidyverse)
data("AssociatedPress")
AssociatedPress %>% tidy()
```

    ## # A tibble: 302,031 × 3
    ##    document term       count
    ##       <int> <chr>      <dbl>
    ##  1        1 adding         1
    ##  2        1 adult          2
    ##  3        1 ago            1
    ##  4        1 alcohol        1
    ##  5        1 allegedly      1
    ##  6        1 allen          1
    ##  7        1 apparently     2
    ##  8        1 appeared       1
    ##  9        1 arrested       1
    ## 10        1 assault        1
    ## # … with 302,021 more rows

``` r
Tidy_text <- AssociatedPress %>% tidy()
```

``` r
# set a seed so that the output of the model is predictable
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda
```

    ## A LDA_VEM topic model with 2 topics.

``` r
#> A LDA_VEM topic model with 2 topics.
```

``` r
library(tidytext)

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics %>% mutate_if(is.numeric, round, 3)
```

    ## # A tibble: 20,946 × 3
    ##    topic term        beta
    ##    <dbl> <chr>      <dbl>
    ##  1     1 aaron          0
    ##  2     2 aaron          0
    ##  3     1 abandon        0
    ##  4     2 abandon        0
    ##  5     1 abandoned      0
    ##  6     2 abandoned      0
    ##  7     1 abandoning     0
    ##  8     2 abandoning     0
    ##  9     1 abbott         0
    ## 10     2 abbott         0
    ## # … with 20,936 more rows

``` r
library(ggplot2)
library(dplyr)
```

``` r
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
```

![](Text-analysis_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
library(tidyr)

beta_wide <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_wide %>% mutate_if(is.numeric, round, 3)
```

    ## # A tibble: 198 × 4
    ##    term           topic1 topic2 log_ratio
    ##    <chr>           <dbl>  <dbl>     <dbl>
    ##  1 administration  0      0.001     1.68 
    ##  2 ago             0.001  0.001    -0.339
    ##  3 agreement       0.001  0.001     0.63 
    ##  4 aid             0      0.001     4.46 
    ##  5 air             0.002  0        -2.85 
    ##  6 american        0.002  0.002    -0.27 
    ##  7 analysts        0.001  0       -10.9  
    ##  8 area            0.001  0        -2.57 
    ##  9 army            0      0.001     2.00 
    ## 10 asked           0      0.002     3.05 
    ## # … with 188 more rows

``` r
#> # A tibble: 198 × 4
#>    term              topic1      topic2 log_ratio
#>    <chr>              <dbl>       <dbl>     <dbl>
#>  1 administration 0.000431  0.00138         1.68 
#>  2 ago            0.00107   0.000842       -0.339
#>  3 agreement      0.000671  0.00104         0.630
#>  4 aid            0.0000476 0.00105         4.46 
#>  5 air            0.00214   0.000297       -2.85 
#>  6 american       0.00203   0.00168        -0.270
#>  7 analysts       0.00109   0.000000578   -10.9  
#>  8 area           0.00137   0.000231       -2.57 
#>  9 army           0.000262  0.00105         2.00 
#> 10 asked          0.000189  0.00156         3.05 
#> # … with 188 more rows
```

``` r
ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents %>% mutate_if(is.numeric, round, 3)
```

    ## # A tibble: 4,492 × 3
    ##    document topic gamma
    ##       <dbl> <dbl> <dbl>
    ##  1        1     1 0.248
    ##  2        2     1 0.362
    ##  3        3     1 0.527
    ##  4        4     1 0.357
    ##  5        5     1 0.181
    ##  6        6     1 0.001
    ##  7        7     1 0.773
    ##  8        8     1 0.004
    ##  9        9     1 0.967
    ## 10       10     1 0.147
    ## # … with 4,482 more rows

11111111hgni= #\> A LDA_VEM topic model with 2 topics.
