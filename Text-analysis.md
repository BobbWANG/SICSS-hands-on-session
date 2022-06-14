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

## I am Personally not Satisfied With the Validation Outcome

We didnt’ replicate the six organizational cultures by using the OCP
following the original authors’ procedure.

One potential reason is that our sample size is not large enough.

Another reason would be that novel people’s (employees’) communication
or experience of organizational cultures is different from employers’.

Whether the dimensions people use to communicate organizational cultures
are different from the employers try to communicate in job
advertisements?

I think the questions with different formats need a more validation step
based on a relative large sample size having relatively good external
validity. That is applying text mining techniques to job advertisements.

## A New Validation Step by Using Structural Topic Modeling

We extracted more descriptions (around 2200 descriptions including the
original sixty). I can compare the culture keywords from the OCP with
the topic keywords returned by the STM.

Because the original 60 descriptions were rated by on average 10 raters,
I can make use of these scores. For example, I can choose a description
and see the extent to which the dominant topics (the keywords related to
them) that the STM characterize converge with the its OCP culture
scores.

The following are the concrete steps I took and some snapshoots of the
results. During the analyssi, I encountered some problems that need your
help and suggestions!

### Read in data

``` r
Organizaitonal_Culture <- readxl::read_xlsx("companies_2ndPartwith1ndPart60.xlsx",na = "",col_names = T, sheet = "Sheet2")
```

### Load packages for data analysis

``` r
library(stm)
```

### Preprocess the documents

``` r
Organizaitonal_culture_Preprocessed <- textProcessor(Organizaitonal_Culture$company_description, metadata = Organizaitonal_Culture,
lowercase = TRUE,
removestopwords = TRUE,
removenumbers = TRUE,
removepunctuation = TRUE,
ucp = FALSE,
stem = TRUE,
wordLengths = c(3, Inf),
sparselevel = 1,
language = "en",
verbose = TRUE,
onlycharacter = FALSE,
striphtml = FALSE,
customstopwords = c("company"),
custompunctuation = NULL,
v1 = FALSE) 
```

    ## Building corpus... 
    ## Converting to Lower Case... 
    ## Removing punctuation... 
    ## Removing stopwords... 
    ## Remove Custom Stopwords...
    ## Removing numbers... 
    ## Stemming... 
    ## Creating Output...

### Plot the number of words and documents removed for different thresholds

``` r
plotRemoved(Organizaitonal_culture_Preprocessed$documents, lower.thresh = seq(1, 201, by = 10))
```

![](Text-analysis_files/figure-markdown_github/unnamed-chunk-1-1.png)

### Select a threshold and preprocess the documents

Qeustions: What are your suggestions for selecting a reasonable
threshold? Setting the threshold as 24 (1% of the number of the total
documents)

``` r
output_thresh24 <- prepDocuments(Organizaitonal_culture_Preprocessed$documents, Organizaitonal_culture_Preprocessed$vocab, lower.thresh = 24)
```

    ## Removing 20753 of 22132 terms (47100 of 228713 tokens) due to frequency 
    ## Your corpus now has 2333 documents, 1379 terms and 181613 tokens.

Setting the threshold as 117 (1% of the number of the total documents)

``` r
output_thresh117 <- prepDocuments(Organizaitonal_culture_Preprocessed$documents, Organizaitonal_culture_Preprocessed$vocab, lower.thresh = 117)
```

    ## Removing 21717 of 22132 terms (100760 of 228713 tokens) due to frequency 
    ## Your corpus now has 2333 documents, 415 terms and 127953 tokens.

### Model selection and search

Searching when setting the threshold as 24.

``` r
model_search_thresh24 <- searchK(output_thresh24$documents, out$vocab, K = (2:50), proportion = 0.3, heldout.seed = 666666, cores = 5)
```

    ## Using multiple-cores.  Progress will not be shown.

### Plot of model selection results

Semantic coherence: the core idea is that in models which are
semantically coherent the words which are most probable under a topic
should co-occur within the same document. Semantic exclusivity: The core
idea is that the top words for a topic are unlikely to appear within top
words of other topics. These two indexes are usually negatively
correlated so that a trade-off must be made. \#### Plot for the model
selection results when the threshold is 24 documents.

``` r
plot(model_search_thresh24)
```

![](Text-analysis_files/figure-markdown_github/unnamed-chunk-5-1.png)

Searching when setting the threshold as 117.

``` r
model_search_thresh117 <- searchK(output_thresh117$documents, output_thresh117$vocab, K = (2:50), proportion = 0.3, heldout.seed = 666666, cores = 5)
```

    ## Using multiple-cores.  Progress will not be shown.

### Plot for the model selection results when the threshold is 117 documents.

``` r
plot(model_search_thresh117)
```

![](Text-analysis_files/figure-markdown_github/unnamed-chunk-7-1.png)
\### Inspect the model indices \#### when the threshold is 24

``` r
model_indices_threshold24 <- as.data.frame(cbind(t(as.data.frame(model_search_thresh24[['results']]$K)), t(as.data.frame(model_search_thresh24[['results']]$exclus)), t(as.data.frame(model_search_thresh24[['results']]$semcoh))))
```

``` r
model_indices_threshold24  %>% rename(Nnumber_of_topics = V1, semantic_coherence = V2, semantic_exclusivity = V3) %>% arrange(semantic_coherence)
```

    ##      Nnumber_of_topics semantic_coherence semantic_exclusivity
    ## X2L                  2           7.518328            -42.09876
    ## X3L                  3           8.486763            -43.02820
    ## X4L                  4           8.850150            -51.89582
    ## X5L                  5           8.948939            -50.02412
    ## X6L                  6           8.988951            -50.43239
    ## X7L                  7           9.105246            -52.24001
    ## X8L                  8           9.270346            -56.01724
    ## X9L                  9           9.411032            -54.57593
    ## X12L                12           9.440370            -67.73229
    ## X11L                11           9.447235            -69.03620
    ## X14L                14           9.508421            -60.25668
    ## X10L                10           9.508844            -67.37928
    ## X13L                13           9.515982            -68.92263
    ## X15L                15           9.573830            -67.05819
    ## X16L                16           9.588443            -68.42804
    ## X17L                17           9.607107            -70.79534
    ## X19L                19           9.633726            -64.71206
    ## X20L                20           9.636732            -64.39921
    ## X26L                26           9.645907            -67.55717
    ## X18L                18           9.646221            -71.22884
    ## X23L                23           9.651970            -66.47508
    ## X21L                21           9.658838            -66.92676
    ## X25L                25           9.664903            -68.22149
    ## X22L                22           9.672905            -66.03079
    ## X27L                27           9.678719            -69.91483
    ## X24L                24           9.680013            -69.78348
    ## X29L                29           9.693803            -71.18053
    ## X28L                28           9.697275            -70.31731
    ## X31L                31           9.703585            -72.41313
    ## X30L                30           9.705764            -71.50157
    ## X33L                33           9.732504            -74.11152
    ## X32L                32           9.733894            -72.84688
    ## X34L                34           9.739771            -72.26233
    ## X38L                38           9.746786            -74.55428
    ## X40L                40           9.752180            -75.73091
    ## X37L                37           9.754612            -73.89920
    ## X39L                39           9.757536            -74.11764
    ## X36L                36           9.760657            -73.43013
    ## X35L                35           9.761187            -72.41692
    ## X43L                43           9.765681            -73.90951
    ## X44L                44           9.767335            -74.02882
    ## X41L                41           9.767390            -74.92393
    ## X42L                42           9.769739            -74.32519
    ## X45L                45           9.771051            -74.78722
    ## X46L                46           9.780582            -77.27170
    ## X49L                49           9.781239            -76.38981
    ## X47L                47           9.781291            -77.02098
    ## X48L                48           9.784727            -75.64237
    ## X50L                50           9.786907            -77.09252

#### when the threshold is 117

``` r
model_indices_threshold117 <- as.data.frame(cbind(t(as.data.frame(model_search_thresh117[['results']]$K)), t(as.data.frame(model_search_thresh117[['results']]$exclus)), t(as.data.frame(model_search_thresh117[['results']]$semcoh))))
```

``` r
model_indices_threshold117  %>% rename(Nnumber_of_topics = V1, semantic_coherence = V2, semantic_exclusivity = V3) %>% arrange(semantic_coherence)
```

    ##      Nnumber_of_topics semantic_coherence semantic_exclusivity
    ## X2L                  2           8.033410            -40.58673
    ## X3L                  3           8.677512            -45.27131
    ## X4L                  4           8.796057            -43.07162
    ## X5L                  5           9.119265            -47.43057
    ## X6L                  6           9.307978            -47.45364
    ## X8L                  8           9.344858            -50.45285
    ## X7L                  7           9.373690            -48.12765
    ## X9L                  9           9.479072            -51.74460
    ## X10L                10           9.523977            -51.42438
    ## X11L                11           9.547642            -52.08919
    ## X12L                12           9.594540            -51.99255
    ## X13L                13           9.610890            -54.05007
    ## X14L                14           9.632120            -53.84729
    ## X16L                16           9.637456            -56.13576
    ## X17L                17           9.639122            -59.63846
    ## X15L                15           9.650038            -56.22887
    ## X18L                18           9.666360            -61.04416
    ## X20L                20           9.688017            -62.01374
    ## X19L                19           9.696281            -64.21681
    ## X23L                23           9.713271            -63.03483
    ## X24L                24           9.715127            -62.86012
    ## X21L                21           9.722107            -63.95700
    ## X22L                22           9.723765            -64.04404
    ## X32L                32           9.731146            -64.69285
    ## X26L                26           9.733123            -63.04437
    ## X31L                31           9.733555            -63.97041
    ## X30L                30           9.738128            -64.18451
    ## X27L                27           9.739501            -63.50200
    ## X25L                25           9.740139            -63.08125
    ## X33L                33           9.742605            -65.14858
    ## X29L                29           9.748035            -63.82543
    ## X28L                28           9.752978            -63.87443
    ## X34L                34           9.753063            -66.05225
    ## X35L                35           9.760183            -65.87990
    ## X36L                36           9.771910            -66.16024
    ## X37L                37           9.788041            -66.51865
    ## X43L                43           9.803141            -67.41028
    ## X44L                44           9.805012            -67.49218
    ## X46L                46           9.805102            -66.58006
    ## X38L                38           9.805816            -67.11276
    ## X42L                42           9.806649            -68.23743
    ## X45L                45           9.806701            -67.41661
    ## X39L                39           9.810238            -66.77796
    ## X40L                40           9.813812            -68.13395
    ## X47L                47           9.818348            -68.51743
    ## X48L                48           9.819409            -68.65429
    ## X49L                49           9.820518            -69.19331
    ## X50L                50           9.820616            -68.64202
    ## X41L                41           9.822102            -67.82505
