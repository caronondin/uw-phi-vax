# dummy data
samples <- c('A','B','C', 'D')
var1 <- c(3, 5, 2, 5)
var2 <- c(4, 4, 2, 2)
var3 <- c(5, 12, 12, 8)
df <- data.frame(var1,var2,var3,row.names=samples)

new.col <- apply(df, 1, prod)

result <- as.data.frame(new.col)

df$result <- result

# take the nth rooth of the new result
n <- 3 # in our index this will be 3
df$result <- df$result^(1/n)
