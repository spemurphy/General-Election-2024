stand <- function(x, y){ 
  z <- (x - mean(y)) / sd(y)
  return(z)
}

politicalPosters %>%
  count(constituency)

fw <- politicalPosters %>%
  subset(constituency == "FW")

fe <- politicalPosters %>%
  subset(constituency == "FE")

dw <- politicalPosters %>%
  subset(constituency == "DW")

dnw <- politicalPosters %>%
  subset(constituency == "DNW")

dbn <- politicalPosters %>%
  subset(constituency == "DBN")

dc <- politicalPosters %>%
  subset(constituency == "DC")

# fw 1, 10:11
# fe 6:9, 12:15
# dw 2:4
# dnw 5, 16:23, 34
# dbn 24:33
# dc 35:37



## What if the non-smiling outlier is removed?
```{r}
politicalPosters %>%
  filter(smile == 0) %>%
  filter(pctVotes > 0.2)
```
Michael Collins is one outlier for the non-smiling politicians, who polled extremely well in Cork South-West (No surprise there). 

Will removing him from the test give a fairer comparison?
  
  ```{r}
which(politicalPosters$name == "Michael Collins")
pp1 <- politicalPosters[-73, ]
```

```{r}
levels(pp1$smile)
wilcox.test(pp1$pctVotes ~ pp1$smile, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F, exact=F, correct=T)
```

Again, the initial hypothesis test holds true. 

```{r}
politicalPosters$smile <- relevel(politicalPosters$smile, ref=1)
wilcox.test(politicalPosters$pctVotes ~ politicalPosters$smile, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F, exact=F, correct=T)
```

Finally, after changing smile (1) to be the reference level, we get the same result in the wilcox.test output. Multiple hypothesis test have been run, so it is important to consider the Bonferonni Correction: There were 4 hypothesis test run at a significane level of 0.05, with each test achieving a p-value of < 0.01. This means that even with potential random chance is considered when conducting multiple hypothesis test, we are still safe to continue. 

Now, it is time to conduct a linear model to understand the predictive capacity of smiling in a campaign poster on the total percentage of votes you receive in your respective constituency. 

```{r}
fit1 <- lm(pctVotes ~ smile, data=politicalPosters)
summary(fit1)
```

When looking at the predictive capacity of smiling on the percentage of votes received, the adjusted r-squared provides a clearer picture. A low r-squared of 0.0730 suggests that a model using "smile" as a predictor of "pctVotes" explains only around 7% of the variance within the variable "pctVotes", indicating poor fit. 

```{r}
fit2 <- lm(pctVotes ~ smile + party + gender, data=politicalPosters)
summary(fit2)
```

politicalPosters$standardisedVotes <- NA
politicalPosters
politicalPosters$standardisedVotes[c(1, 10, 11)] <- stand(fw[1:3,6], fw$votes) 
politicalPosters$standardisedVotes[c(6:9, 12:15)] <- stand(fe[1:8,6], fe$votes) 
politicalPosters$standardisedVotes[c(2:4)] <- stand(dw[1:3,6], dw$votes) 
politicalPosters$standardisedVotes[c(5, 16:23, 34)] <- stand(dnw[1:10,6], dnw$votes) 
politicalPosters$standardisedVotes[c(24:33)] <- stand(dbn[1:10,6], dbn$votes) 
politicalPosters$standardisedVotes[c(35:37)] <- stand(dc[1:3,6], dc$votes) 
politicalPosters$standardisedVotes