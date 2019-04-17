# Print the first rows of the data
head(comics, n=1)
print(comics)
# Check levels of align
levels(comics$align)

# Check the levels of gender
levels(comics$gender)

# Create a 2-way contingency table
tab <- xtabs(~ align+gender, data=comics)
tab


# Load dplyr
library(dplyr)
# Print tab
print(tab)

# Remove align level
comics <- comics %>%
  filter(align != "Reformed Criminals") %>%
  droplevels()

# See the result
comics_filtered <- comics


# Load ggplot2
library(ggplot2)

# Create side-by-side barchart of gender by alignment
ggplot(comics, aes(x = align, fill = gender)) + 
  geom_bar(position = 'dodge')

# Create side-by-side barchart of alignment by gender
ggplot(comics, aes(x = gender, fill = align)) + 
  geom_bar(position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90))