# DATA COLLECTION

Data were collected in *python* using the functions below. Full Python scripts are available here. Original data _and_ cleaned CSVs are available in the RedditS20 Google Drive. 

**Post collection**
`getcomments('SUBREDDITNAME')`

**Comment collection**
`getcomments('SUBREDDITNAME')`

# R FUNCTIONS
R code for these functions is available [here](https://github.com/sebolden/reddit/blob/master/all_functions.r).

* `getcsv(string)`
  * `string`: The name of a CSV file in string format, e.g. `comments.csv`
  * Literally just a shortcut function for `read.csv()`

## Data cleaning & prepping

### Quick access 
```{r}
# general analysis 
data <- makeBabyDF(getcsv('trp_nw_full.csv'))

# text analysis
data <- subsetQuantiles(data, .95, .05, .05)
canon <- getcsv('text_files_combined.csv')
combined <- combineCommentsAndCanon(data, canon)
```
### Details (building CSVs & explaining functions)

Comments were cleaned using the below set of functions:

* `cleanAllComments(all_data)`
  * `all_data`: An original comment CSV -- that is, one where all the subreddit comments have been combined, but have not undergone additional preprocessing steps. 
  * Strips away unnecessary variables; standardizes dummy and factor variables; creates new dummy variables for deleted/removed comments; converts epoch 'created' utc to readable date format; adds a variable to ID comment year; removes glitchy data, e.g. several rows with innacurate subreddit names.
  
*  `makeBabyDF(df)`
  * `df`: A data frame that has been passed through `cleanAllComments()`. 
  * Main purpose is to create a smaller DF with fewer columns, making it easier for R to handle on computers with limited RAM.Filters larger comment DF to only include the following variables: date, year, author, id, link_id, parent_id, subreddit, score, body.  
  * Secondary purpose is to standardize subsequent analysis by removing deleted/removed comments and authors from the data frame (that is, data processed in textual analysis is the same data used in network analysis). 
  
* `subsetQuantiles(df, up_q, low_q, perc)`
  * `df`: Comment data frame, ideally one that's been passed through `makeBabyDF()`.
  * `up_q`: Upper quantile, e.g. 0.95 represents the top 5% of comments (by score) for a given subreddit.
  * `low_q`: Lower quantile, e.g. 0.05 represents the bottom 5% of comments (by score) for a given subreddit.
  * `perc`: Percentage of subsetted comments to sample from a given subreddit, e.g. 0.05 will take 5% of the comments from a subreddit's upper quantile and 5% of comments from a subreddit's lower quantile. 
  * Used to systematize the sampling process for text analysis. Though the quantiles and sample % can be customized, the function is designed to randomly sample a set of comments from the top X% (highest-scoring) and bottom X% (lowest-scoring) comments from each subreddit. A sample percentage is included in addition to the quantile values to make it easy to adjust the volume of comments that are sampled; as with other functions, this is mainly to prevent my computer from trying to murder me for using all of my available RAM during my analysis.
  
* `combineCommentsAndCanon(commentDF, txtDF)`
   * `commentDF`: Comment data frame, ideally one that has been passed through `makeBabyDF()`. 
   * `txtDF`: Data frame with canon documents
 
 ## Data analysis
 
 ### Text
 some words and things will end up here
 
 ### Network
additional things n words will go here
   
   
   
