library(pcaMethods)
snp_data = read.csv("https://stuntspt.gitlab.io/asb2022/classes/class_14/assets/TLE.str",
                    header=TRUE,
                    row.names=1,
                    sep=" ")
View(snp_data)

tlep_pca = pca(snp_data[,-c(1, 2, 3)],
               scale="none",
               center=T,
               nPcs=2,
               method="nipals")

# We have to use the `nipals` method because we have missing data
# We are also skipping columns 1 and 2 since the contain the population name and number respectively

print(tlep_pca@R2)

# Use the age group as discriminant
slplot(tlep_pca,
       scol=snp_data[, "Age"],
       scoresLoadings=c(TRUE,FALSE),
       sl=NULL,  # What does this do?
       spch="x")

legend("bottomright",
       legend=sort(unique(snp_data[, "Age"])),
       col=sort(unique(snp_data[, "Age"])),
       pch="x")