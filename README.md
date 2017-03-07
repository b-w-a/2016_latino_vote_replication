# 2016 Latino Voter Replication File 

## This contains the data and code to replicate the results in the following studies 

### Links to Articles
+ Florida (Valenzuela and Reny): http://thehill.com/blogs/pundits-blog/presidential-campaign/310760-study-finds-trump-faired-worse-than-romney-with

+ New York (Garcia-Rios and Reny): http://www.nydailynews.com/opinion/no-trump-didn-surprisingly-latino-voters-article-1.2944772

+ Illinois (Dominguez and Reny): http://www.huffingtonpost.com/entry/in-contrast-to-the-exit-poll-official-election-results_us_589ca858e4b0985224db5e6e 

+ California (Pedraza and Wilcox-Archuleta): http://www.latimes.com/opinion/op-ed/la-oe-pedraza-latino-vote-20170111-story.html

+ Arizona (Nuño and Wilcox-Archuleta): http://www.azcentral.com/story/opinion/op-ed/2016/11/26/exit-polls-wrong-latino-voters-arizona/94288570/

+ Texas (Pedraza and Wilcox-Archuleta): https://www.washingtonpost.com/news/monkey-cage/wp/2016/12/02/donald-trump-did-not-win-34-of-latino-vote-in-texas-he-won-much-less/

+ Colorado (Griffin and Wilcox-Archuleta): http://thehill.com/blogs/pundits-blog/presidential-campaign/318751-election-autopsy-latinos-favored-clinton-more-than

+ Nevada (Pedraza and Wilcox-Archuleta): https://thenevadaindependent.com/article/precinct-returns-prove-exit-polls-wrong-latino-vote

### Data acquisition and availability 

+ The goal of the data collection process was to obtain reliable precinct level demographic characteristics, namely the percent of Latinos within the voting precinct, the vote share for both Clinton and Trump, and the total number of votes cast in the precinct. 

+ For the most part, precinct level vote returns were obtained directly from the county clerks of the respective counties. Often times these were made available via a canvass report posted on the website. State specific sources are available via the individual state pages below. 

+ Precinct level demographics were obtained from two sources depending on the state. In some states, comprehensive publicly available redistricting data with precinct level demographics were used. In states where this information was not available, we used Catalist, a private online vendor, to supply the demographic estimates.

### Ecological Inference

+ The point of ecological inference is to solve the ecological fallacy of inferring individual level behavior from aggregate data. King's (1997) method is the most widely used ecological inference method in academia and is extensively used in voting rights cases. There is extensive documentation about the reliability and accuracy of these estimates is available here: http://gking.harvard.edu/category/research-interests/methods/ecological-inference 


### Code Examples


This script contains sample code to create homogenous precinct analysis plots and to run King's EI on the 2016 election data.

```{r}
#install packages if needed
#install.packages('eiCompare') #for EI
#install.packages('dplyr') #for summarising data
#install.packages('ggplot2') #for graphics
#install.packages('tidyr') #for reshaping dataframe
#install.packages('readr') #for reading in data

#load packages
library(eiCompare)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

# read in the data
# directly from github
df <- read_csv('https://github.com/b-w-a/2016_latino_vote_replication/raw/master/2016_final_replication.csv')

# look at the data
glimpse(df)
```

Each row in the dataset is a precinct within a state. For more information on the counties where the precincts reside, see individual files on https://github.com/tylerreny/election2016 for FL, IL, NY or https://github.com/b-w-a?tab=repositories for CO, NV, CA, NM, TX, and AZ.

# Homogenous precinct analysis

For this example I first group by state, create weights so that each precinct is weighted by the proportion of voters in each precinct in the state, then I filter, for this example, to just show those precincts in California. Gather turns the dataframe from wide to long for ggplot and then I plot the precincts, change the color scheme, add a weighted GAM for each candidate, and fix the axis labels. 

```{r}
df %>% 
  group_by(state) %>% 
  mutate(grand_total=sum(total_votes,na.rm=T),
         wt=total_votes/grand_total) %>%
  select(-grand_total) %>%
  na.omit() %>%
  filter(state=='ca') %>%
  gather(Candidate,Votes,3:4) %>%
  ggplot(aes(pct_latino,Votes,color=Candidate,size=wt)) + 
  geom_point(alpha=0.05) + guides(size=F) +
  scale_color_manual(values = c('blue','red'),
                     label=c('Clinton','Trump')) +
  stat_smooth(aes(weight=wt,color=Candidate),se=F) +
  labs(title='Latino presidential vote in California',
       subtitle='plotted by each weighted precinct',
       x="Pct Latino in each precinct", y='Pct Total Vote') +
  scale_x_continuous(limits=c(0,1)) +
  theme_bw()
```

# Ecological Inference

For the ecological inference, we need to first create a few new columns in the data to create a full EI results table. For this example, I filter the data to just California and randomly sample from that for the example. Running EI on the full dataset takes more than 10 hours. Each state separately can be run in just a few hours.

```{r}

# first filter by the state you want 
# or use the full dataset by deleting the filter argument
# and create the columns needed for a full King's EI table
# note we sample 500 rows from the data to run this quickly,
# but you will want to get rid of that line of code to run on the whole thing

california <- df %>% 
  filter(state=='ca') %>%
  sample_n(500) %>% #get rid of this
  mutate(pct_other = 1 - (pct_trump+pct_clinton),
         pct_nonlatino = 1-pct_latino)

# this code establishes the candidates and groups (for the different EI runs)
# then runs EI using the eiCompare package (which is just a simplifying wrapper for Kings EI)

cands <- c("pct_clinton", "pct_trump", "pct_other")
groups <- c("~ pct_latino", "~ pct_nonlatino") 
table_names <- c("EI: Pct Latino", "EI: Pct Non Latino")

results <- ei_est_gen(cands, groups, "total_votes", 
                      data = california, 
                      table_names = table_names,
                      beta_yes = T)
main_table <- results[[1]]
main_table
```

We can also extract the estimated Latino vote for each precinct to construct a density plot for each candidate with vertical lines indicating their means.

```{r}
# extract betas from results list
betas <- results[[2]]

# extract means for plot
Clinton_Estimate <- main_table[1,2]/100
Trump_Estimate <- main_table[3,2]/100

# make long dataframe and plot two densities together with estimated means
gather(betas,Candidate,Betas,c(1,3)) %>%
  ggplot(aes(Betas, fill=Candidate)) + geom_density(alpha=0.1) +
  scale_color_manual(values=c('blue','red')) +
  scale_fill_manual(values=c('blue','red'),labels=c('Clinton','Trump')) +
  labs(x='Estimated Latino Vote Share') +
  theme_bw() +
  geom_vline(xintercept=c(Clinton_Estimate,Trump_Estimate),
             color=c('Blue','Red'))
```



