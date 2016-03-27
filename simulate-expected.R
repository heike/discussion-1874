# getting an initial set of probabilities:
dframe <- data.frame(matrix(data=runif(B*4), ncol=4))
dframe$sum <- rowSums(dframe) 
dframe <- dframe[,-5]/dframe$sum
names(dframe) <- c("p11", "p12", "p21", "p22")
dframe$id <- 1:nrow(dframe)
dm <- tidyr::gather(dframe, group, p, -id)
dm <- dm %>% group_by(id) %>% mutate(group = group[sample(4,4,replace=FALSE)])
qplot(data=dm, x=p, facets=~group, binwidth=0.05)


# expected values based on independence
library(tidyr)
ds <- spread(dm, group, p)
ds <- ds %>% transform(
  p1 = p11 + p12,
  p2 = p11 + p21
)
ds <- ds %>% transform(
  e11 = p1*p2,
  e12 = p1*(1-p2),
  e21 = (1-p1)*p2,
  e22 = (1-p1)*(1-p2)
)

obs <- gather(ds[,1:5], group, obs, -id)
exp <- gather(ds[,c(1,8:11)], group, exp, -id)
exp$group <- gsub("e","", exp$group)
obs$group <- gsub("p","", obs$group)

tabs <- merge(obs, exp, by=c("id", "group"))
tabs$resid <- with(tabs, (obs-exp)/sqrt(exp))
qplot(resid, data=tabs, facets=~group)
qplot(resid, data=tabs)

qqnorm(tabs$resid) # lower tail is too light - that's to be expected. 
