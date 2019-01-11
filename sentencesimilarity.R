### Document similarity

library(stringr)
library(text2vec)
library(dplyr)

### preprocessing of OADA act

aodaactdfnew = aodaactdf[1:660,1]

prep_fun = function(x) {
  x %>% 
    # make text lower case
    str_to_lower %>% 
    # remove non-alphanumeric symbols
    str_replace_all("[^[:alnum:]]", " ") %>% 
    # collapse multiple spaces
    str_replace_all("\\s+", " ") %>%
    str_replace_all("[[:digit:]]+", " ")
}

aodaactdf$clean = prep_fun(aodaactdf$d1)

### preprocessing of ODA act

aodaactdfnew = odaactdf[1:310,1]

odaactdf$clean = prep_fun(odaactdf$d2)

doc_set_1 = aodaactdf[1:660, ]
it1 = itoken(doc_set_1$clean, progressbar = FALSE)

# specially take different number of docs in second set
doc_set_2 = odaactdf[1:310, ]
it2 = itoken(doc_set_2$clean, progressbar = FALSE)


it11 = itoken(aodaactdf$clean, progressbar = FALSE)
v1 = create_vocabulary(it11) %>% prune_vocabulary(doc_proportion_max = 0.1, term_count_min = 5)
vectorizer = vocab_vectorizer(v1)

it11 = itoken(aodaactdf$clean, progressbar = FALSE)
v1 = create_vocabulary(it11) %>% prune_vocabulary(doc_proportion_max = 0.1, term_count_min = 5)
vectorizer = vocab_vectorizer(v1)

# they will be in the same space because we use same vectorizer
# hash_vectorizer will also work fine

dtm1 = create_dtm(it1, vectorizer)
dim(dtm1)

dtm2 = create_dtm(it2, vectorizer)
dim(dtm2)

d1_d2_jac_sim = sim2(dtm1, dtm2, method = "jaccard", norm = "none")

dim(d1_d2_jac_sim)

d1_d2_jac_sim[1:2, 1:2]

class(d1_d2_jac_sim)

dfsim <- as.data.frame(as.matrix(d1_d2_jac_sim))

which(as.matrix(d1_d2_jac_sim) == max(as.matrix(d1_d2_jac_sim)), arr.ind = TRUE)

write.csv(dfsim,'simmatrix.csv')


########## cosine similarity

d1_d2_cos_sim = sim2(dtm1, dtm2, method = "cosine", norm = "l2")

dim(d1_d2_cos_sim)

dfsimcosine <- as.data.frame(as.matrix(d1_d2_cos_sim))

write.csv(dfsimcosine,'simmatrixcosine.csv')
                             