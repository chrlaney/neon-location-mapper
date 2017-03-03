# download_locations.R

#### gets full named location catalog ####

pr <- fromJSON("http://data.neonscience.org/api/v0/products")
pr1 <- pr[['data']]
pr2 <- pr1[which(pr1$siteCodes != "NULL"), ]
saveRDS(pr2, "data/products.rds")

psm <- data.frame(product = NA, sites = NA, months = 0)
sites <- character()
for(i in 1:nrow(pr2)){
  p <- pr2$productName[i]
  s <- length(unlist(pr2$siteCodes[[i]]$siteCode))
  m <- length(unlist(pr2$siteCodes[[i]]$availableMonths))
  psm <- rbind(psm, data.frame(product = p, sites = s, months = m))
  sites <- c(sites, unlist(pr2$siteCodes[[i]]$siteCode))
}
psm <- psm[-1,]