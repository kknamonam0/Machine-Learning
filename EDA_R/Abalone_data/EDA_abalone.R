# load libraries
library(tidyverse)
library(skimr) # summary data package
library(corrplot) # Heat map of cor mat package
library(e1071) # Skewness & SVM package
library(GGally) # ggpairs package
library(factoextra) # PCA package
library(gridExtra) # Arrange plots package
library(flextable) # table package
library(car) # VIF package
# ___________________________________________________________


# Load data
setwd("/Users/nam0_k/Desktop/Git_repo/Machine-Learning/EDA/Abalone_data/") # check directory
abalone <- read_csv("abalone1.data.txt", col_names = T)
glimpse(abalone)


# skim 함수를 사용하여 데이터 생성
skim(abalone) |>
    select(
        -contains("character.min"), -contains("character.max"),
        -contains("character.empty"), -contains("character.n_unique"),
        -contains("character.whitespace"), -contains("complete_rate"),
        -contains("skim_type"), -contains("numeric.hist")
    ) |>
    flextable() |>
    highlight(i = 4, j = 5) |>
    autofit()

# 결측치 및 이상치 처리
abalone |>
    filter(Height == 0) |>
    flextable() |>
    highlight(j = 4) |>
    autofit()

abalone |>
    group_by(Sex) |>
    summarise(mean_height = mean(Height)) |>
    flextable() |>
    highlight(i = 2) |>
    autofit()

# 평균값 대체
ind <- which(abalone$Height == 0)
abalone$Height[ind] <- rep(.1079955, 2)


abalone |>
    ggpairs(
        mapping = aes(color = Sex, alpha = 0.5),
        upper = list(continuous = wrap("points", size = .5, alpha = 0.5)),
        lower = list(continuous = wrap("points", size = .5, alpha = 0.5)),
        diag = list(continuous = wrap("barDiag", bins = 15, alpha = 0.5)),
        columnLabels = c(
            "Sex", "Length", "Diameter", "Height",
            "Whole", "Shucked", "Viscera", "Shell", "Rings"
        )
    )


ggplot(mapping = aes(x = Height, y = Rings), data = abalone) +
    geom_point()

abalone <- abalone |> filter(Height < .3)


abalone |>
    select_if(is.numeric) |>
    summarise_all(list(skewness = skewness)) |> ## e1071의 skewness
    gather(Features, Skewness) |> # tidyr의 gather
    arrange(desc(Skewness)) |>
    flextable() |>
    highlight(j = 2) |>
    autofit()

# EDA

## Sex
ggplot(abalone, aes(x = Sex, y = Rings, fill = Sex)) +
    geom_boxplot() +
    scale_fill_brewer(palette = "Pastel1") +
    theme_minimal() +
    labs(
        title = "Boxplot of Rings by Sex",
        x = "Sex",
        y = "Rings"
    )

abalone |>
    group_by(Sex) |>
    summarise(
        count = n(),
        mean = mean(Rings, na.rm = TRUE),
        median = median(Rings, na.rm = TRUE),
        sd = sd(Rings, na.rm = TRUE)
    ) |>
    flextable() |>
    highlight(i = c(1, 3), j = c(3, 5)) |>
    autofit()


## Length
# 첫 번째 플롯: 히스토그램
p1 <- ggplot(abalone, aes(x = Length)) +
    geom_histogram(fill = "4", color = "black", bins = 10) +
    theme_minimal() +
    labs(
        title = "Histogram of Length",
        x = "Length",
        y = "Frequency"
    )

# 두 번째 플롯: 박스 플롯
p2 <- ggplot(abalone, aes(y = Length)) +
    geom_boxplot(fill = "4", color = "black") +
    theme_minimal() +
    labs(
        title = "Distribution of Length",
        y = "Length"
    ) +
    coord_flip()

# 두 개의 플롯을 한 그리드에 배치
grid.arrange(p1, p2, ncol = 2)

summary(abalone$Length)


## Height
# 첫 번째 플롯: 히스토그램
p1 <- ggplot(abalone, aes(x = Height)) +
    geom_histogram(fill = "4", color = "black", bins = 10) +
    theme_minimal() +
    labs(
        title = "Histogram of Height",
        x = "Height",
        y = "Frequency"
    )

# 두 번째 플롯: 박스 플롯
p2 <- ggplot(abalone, aes(y = Height)) +
    geom_boxplot(fill = "4", color = "black") +
    theme_minimal() +
    labs(
        title = "Distribution of Height",
        y = "Height"
    ) +
    coord_flip()

# 두 개의 플롯을 한 그리드에 배치
grid.arrange(p1, p2, ncol = 2)
summary(abalone$Height)


## Diameter
# 첫 번째 플롯: 히스토그램
p1 <- ggplot(abalone, aes(x = Diameter)) +
    geom_histogram(fill = "4", color = "black", bins = 10) +
    theme_minimal() +
    labs(
        title = "Histogram of Diameter",
        x = "Diameter",
        y = "Frequency"
    )

# 두 번째 플롯: 박스 플롯
p2 <- ggplot(abalone, aes(y = Diameter)) +
    geom_boxplot(fill = "4", color = "black") +
    theme_minimal() +
    labs(
        title = "Distribution of Diameter",
        y = "Diameter"
    ) +
    coord_flip()

# 두 개의 플롯을 한 그리드에 배치
grid.arrange(p1, p2, ncol = 2)
summary(abalone$Diameter)


abalone |>
    filter(Length < 0.1 | Diameter < 0.1 | Height < 0.01) |>
    flextable() |>
    autofit()



# Density plot function
plot_density <- function(data, weight_var, fill_var) {
    ggplot(data, aes(x = !!sym(weight_var), fill = !!sym(fill_var))) +
        geom_density(alpha = 0.6) +
        theme_minimal() +
        labs(
            title = paste("Density of", weight_var, "by", fill_var),
            x = weight_var,
            y = "Density"
        )
}


#  Density plot 그리기
p1 <- plot_density(abalone, "Length", "Sex")
p2 <- plot_density(abalone, "Diameter", "Sex")
p3 <- plot_density(abalone, "Height", "Sex")
grid.arrange(p1, p2, p3, ncol = 2)


## Whole weight
# 첫 번째 플롯: 히스토그램
p1 <- ggplot(abalone, aes(x = Whole_weight)) +
    geom_histogram(fill = "4", color = "black", bins = 10) +
    theme_minimal() +
    labs(
        title = "Histogram of Whole weight",
        x = "Whole weight",
        y = "Frequency"
    )

# 두 번째 플롯: 박스 플롯
p2 <- ggplot(abalone, aes(y = Whole_weight)) +
    geom_boxplot(fill = "4", color = "black") +
    theme_minimal() +
    labs(
        title = "Distribution of Whole weight",
        y = "Whole weight"
    ) +
    coord_flip()

# 두 개의 플롯을 한 그리드에 배치
grid.arrange(p1, p2, ncol = 2)

summary(abalone$Whole_weight) # min max의 차이가 크다.


abalone |>
    filter(Whole_weight > 2.5) |>
    flextable() |>
    highlight(j = 5) |>
    autofit()


# Apply function
p1 <- plot_density(abalone, "Whole_weight", "Sex")
p2 <- plot_density(abalone, "Shucked_weight", "Sex")
p3 <- plot_density(abalone, "Viscera_weight", "Sex")
p4 <- plot_density(abalone, "Shell_weight", "Sex")

grid.arrange(p1, p2, p3, p4, ncol = 2)


### 무게의 차이에 대한 정보
diff <- abalone |>
    mutate(weight_diff = Whole_weight - (Shell_weight + Shucked_weight + Viscera_weight))

summary(diff$weight_diff)

diff |>
    filter(weight_diff < 0)


abalone |>
    filter(Whole_weight < Shell_weight | Whole_weight < Shucked_weight | Whole_weight < Viscera_weight) |>
    flextable() |>
    highlight(j = 5) |>
    highlight(i = c(1, 2, 3, 4), j = 6) |>
    highlight(i = 5, j = 8) |>
    autofit()


abalone <- abalone |>
    filter(!(Whole_weight < Shell_weight | Whole_weight < Shucked_weight | Whole_weight < Viscera_weight))


dim(abalone)


## Heatmap of correlation matrix
corrplot(abalone |> select_if(is.numeric) |> cor(),
    insig = "blank",
    tl.cex = .8, # 텍스트 크기
    tl.col = "black",
    method = "color",
    order = "hclust",
    addCoef.col = "black", # 계수 색상
    number.cex = .8, # 계수 텍스트 크기
    type = "full", # 모두 표시
    is.corr = F # 상관 행렬이 input이기 때문에 값의 변화는 없음.
)


sex <- abalone$Sex

abalone |>
    select(where(is.numeric)) |>
    ggpairs(
        mapping = aes(color = sex, alpha = 0.5),
        upper = list(continuous = wrap("points", size = .5, alpha = 0.5)),
        lower = list(continuous = wrap("points", size = .5, alpha = 0.5)),
        diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
        columnLabels = c("Length", "Diameter", "Height", "Whole", "Shucked", "Viscera", "Shell", "Rings")
    )


## Pairwise plot
plot_with_loess <- function(df, x_var, y_var = "Rings") {
    ggplot(df, aes_string(x = x_var, y = y_var)) +
        geom_point(size = .5, alpha = .6) +
        geom_smooth(method = "loess") +
        scale_y_continuous(breaks = seq(min(df[[y_var]]), max(df[[y_var]]), by = 5)) +
        theme_minimal()
}


p1 <- plot_with_loess(abalone, "Length")
p2 <- plot_with_loess(abalone, "Diameter")
p3 <- plot_with_loess(abalone, "Height")
p4 <- plot_with_loess(abalone, "Whole_weight")
p5 <- plot_with_loess(abalone, "Shell_weight")
p6 <- plot_with_loess(abalone, "Shucked_weight")
p7 <- plot_with_loess(abalone, "Viscera_weight")

grid.arrange(p1, p2, p3, p4, p5, p6, p7, ncol = 3)


## 범주데이터 처리
ggplot(abalone, aes(x = Sex, y = Rings, fill = Sex)) +
    geom_boxplot() +
    scale_fill_brewer(palette = "Set2") +
    scale_y_continuous(breaks = seq(0, 30, by = 5))


abalone_s <- abalone |>
    mutate(SexInfant = ifelse(Sex == "I", 1, 0)) |>
    select(-Sex)

head(abalone_s) |>
    flextable() |>
    autofit()



## Rings 데이터의 분류
summary(abalone$Rings)

abalone |>
    ggplot(aes(x = Rings)) +
    geom_boxplot(fill = "4", color = "black") +
    theme_minimal() +
    labs(
        title = "Frequency of Rings",
        x = "Rings",
        y = "Frequency"
    )


abalone |>
    select(Rings) |>
    group_by(Rings) |>
    summarise(count = n()) |>
    arrange(desc(count))


## 두집단 분류
class2 <- abalone_s |>
    mutate(Rings = case_when(
        Rings %in% 1:9 ~ 0,
        Rings %in% 10:30 ~ 1
    ))

write_csv(class2, "class2_basic.csv")

## 세 집단 분류
class3 <- abalone_s |>
    mutate(Rings = case_when(
        Rings %in% 1:5 ~ 0,
        Rings %in% 6:12 ~ 1,
        Rings %in% 13:30 ~ 2
    ))

write_csv(class3, "class3_basic.csv")


class2 |>
    select(Rings) |>
    group_by(Rings) |>
    summarise(count = n()) |>
    arrange(desc(count)) |>
    flextable() |>
    autofit()

class3 |>
    select(Rings) |>
    group_by(Rings) |>
    summarise(count = n()) |>
    arrange(desc(count)) |>
    flextable() |>
    autofit()




# PCA
abalone_pca <- class2


## LDH PCA
ld_pca <- princomp(abalone_pca[, c("Length", "Diameter", "Height")])
summary(ld_pca)

screeplot(ld_pca, type = "line", main = "Scree Plot of PCA")

abalone_pca$LD_PC1 <- ld_pca$scores[, 1]


abalone_pca <- abalone_pca |>
    select(-c("Length", "Diameter", "Height"))

head(abalone_pca)


## Weight PCA
weight_pca <- princomp(abalone_pca[, c("Whole_weight", "Shucked_weight", "Viscera_weight", "Shell_weight")])
summary(weight_pca)

screeplot(weight_pca, type = "line", main = "Scree Plot of PCA")

abalone_pca$W_PC1 <- weight_pca$scores[, 1]
abalone_pca$W_PC2 <- weight_pca$scores[, 2]

abalone_pca <- abalone_pca |>
    select(-c("Whole_weight", "Viscera_weight", "Shucked_weight", "Shell_weight"))

head(abalone_pca)

# Heat map of correlation matrix
corrplot(abalone_pca |> select_if(is.numeric) |> cor(),
    insig = "blank",
    tl.cex = .8, # 텍스트 크기
    tl.col = "black",
    method = "color",
    order = "hclust",
    addCoef.col = "black", # 계수 색상
    number.cex = .8, # 계수 텍스트 크기
    type = "full", # 모두 표시
    is.corr = F # 상관 행렬이 input이기 때문에 값의 변화는 없음.
)


model <- lm(Rings ~ ., data = abalone_pca)
vif(model)

write.csv(abalone_pca, "class2_pca.csv")



# 결론

# -   EDA를 통해 데이터를 파악하고, 이상치 및 결측치를 제거해주었습니다.

# -   다중공선성이 문제될 수 있기 때문에, PCA를 통해서 해결하고자 했습니다.

#    1.  2개의 범주로 나눈 "class2_basic.csv"
#    2.  3개의 범주로 나눈 "class3_basic.csv"
#    3.  2개의 범주로 나누고 PCA를 진행한 "class2_pca.csv"
#    4.  3개의 범주로 나누고 PCA를 진행한 "class3_pca.csv"
