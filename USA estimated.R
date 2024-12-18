install.packages("plm")
install.packages("cli")

library(plm)
library(cli)


# prep data ---------------------------------------------------------------

r <- p; r[,3:11] <- NA
for(i in 3:10) r[,i] <- log(p[,i]/p[,i+1])
rm(p)

ticks <- Reduce(intersect, list(r$tick, scope1$tick, scope2$tick, scope3$tick))
r <- r[which(r$tick %in% ticks),]
scope1 <- scope1[which(scope1$tick %in% ticks),]
scope2 <- scope2[which(scope2$tick %in% ticks),]
scope3 <- scope3[which(scope3$tick %in% ticks),]

names <- r$name

prep <- function(df){
  df_name <- deparse(substitute(df))
  print(df_name)
  rownames(df) <- ticks
  df$tick <- df$name <- NULL
  df <- reshape2::melt(as.matrix(df))
  colnames(df) <- c("tick", "year", df_name)
  df$year <- as.numeric(stringr::str_remove(df$year, "X"))
  df[,3] <- as.numeric(as.character(df[,3]))
  return(df)  
}

scope1 <- prep(scope1)
scope2 <- prep(scope2)
scope3 <- prep(scope3)
r <- prep(r)

df <- r
df$scope1 <- scope1$scope1
df$scope2 <- scope2$scope2
df$scope3 <- scope3$scope3


df <- df[which(rowSums(is.na(df[,4:6])) < 3), ]
df <- df[which(!is.na(df$r)), ]


# panel reg ---------------------------------------------------------------

panel_df <- pdata.frame(df, index = c("tick", "year"))

mdl <- plm(r ~ scope1 + scope2 + scope3, data = panel_df, model = "within")

summary(df)

summary(mdl)

