### Name: bronch
### Title: Bronchial carcinoma data
### Aliases: bronch
### Keywords: datasets

### ** Examples

data(bronch)
# raw tumour counts:

table(bronch[c("group","Y")])

# groupwise times of death:

boxplot(time ~ group, data=bronch, horizontal=TRUE)




