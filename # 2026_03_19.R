# 2026-03-19
# CCB

#load in data via relative path
load("data/RAMLDB v4.66/R Data/DBdata[asmt][v4.66].RData")

# load stuffs
library(tidyverse)

data1 = data.frame(ID = c(1,2), X1 = c("a1", "a2"))
data2 = data.frame(ID = c(2,3), X2 = c("b1", "b2"))
data1
data2

data12_left = left_join(data1, data2)
data12_left

data12_left = data1 %>%
    left_join(data2) #preferred 

# don't use, it is A LOT less intuitive 
data12_right = data1 %>%
    right_join(data2)
data12_right

data12_inner = data1 %>%
    inner_join(data2)
data12_inner

data12_full = data1 %>%
    full_join(data2)
data12_full

data12_semi = data1 %>%
    semi_join(data2)
data12_semi

# OR join by a specific column
data12_semi = data1 %>%
    semi_join(data2, by = "ID")
data12_semi

data12_anti = data1 %>%
    anti_join(data2, by = "ID")
data12_anti

# row duplication! BIG PROB, BE CAREFUL AND CHECK UR DIMENSIONS ie dim()

# what would you do if your find that row duplicated? Can you delete it? Would you look at the papers to determine which is the better length estimate?


# data pivots
survey = data.frame(quadrat_ID = c(101, 102, 103, 104),
                      barnacle = c(2, 11, 8, 27),
                      chiton = c(1, 0, 0, 2),
                    mussel = c(0, 1, 1, 4))
survey

# used to be gather() and spread()
long = survey %>%
    pivot_longer(cols = c("barnacle", "chiton", "mussel"), names_to = "taxa", values_to = "count")
long

wide = long %>%
    pivot_wider(names_from = taxa, values_from = count)
wide
survey

# long 
ggplot(data = long) +
    geom_point(aes(x = quadrat_ID, y = count, color = taxa))

ggplot(data = wide) +
    geom_point(aes(x = quadrat_ID, y = barnacle), color = "pink") +
  geom_point(aes(x = quadrat_ID, y = chiton), color = "green") +
  geom_point(aes(x = quadrat_ID, y = mussel), color = "cyan")
