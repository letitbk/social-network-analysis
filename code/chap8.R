####################################
#               제 8 장             #
####################################
# install.packages(c('sna', 'network', 'intergraph', 'extrafont'))
library(sna)
library(igraph)
library(network)
library(intergraph)
library(ggplot2)
library(dplyr)
library(extrafont)

# extrafont::font_import(pattern = 'NanumGothic.ttf', prompt = FALSE)
par(family = 'NanumGothic', mar = c(0, 0, 0, 0))

## 구조적 등위성
### 병원 관계 데이터에서 row.names을 제거하고 행렬로 변환하기 
hosp <- read.csv('files/hospital.csv')
hosp_m <- as.matrix(dplyr::select(hosp, -1))
### network 객체로 변환하기  
net1 <- network::network(hosp_m, directed = TRUE)
### structural equivalence 계산 
eq  <- equiv.clust(net1, 'default')
plot(eq)
### 해밍 거리 기반 구조적 등위성 계산  
g.se.hamming <- sedist(net1, method = 'hamming') 
g.se.hamming
### 유클리드 거리 기반 구조적 등위성 계산 
g.se.euclidean <- sedist(net1, method = 'euclidean')
round(g.se.euclidean, 2)
### 유클리드 거리에 기반한 구조적 등위성 덴드로그램 (Single 방식)
eq_euclidean <- equiv.clust(g.se.euclidean, cluster.method = 'single')
plot(eq_euclidean)
### 병원 내 역할 관계를 구조적 등의성 산출 후 MDS 방식으로 시각화하기 
#install.packages('ggrepel')
library(ggrepel)
mds.graph <- data.frame(cmdscale(as.dist(g.se.euclidean)))
names <- 1:10
ggplot(mds.graph, aes(X1, X2, label=names)) +  geom_text_repel() +
  theme_bw() 

## 블록모델링
### 이탈리아 중세 가문 간 결혼 데이터 입력하기 
g <- graph_from_literal(Peruzzi-Bischeri,
      Peruzzi-Strozzi,
      Peruzzi-Castellani,
      Bischeri-Guadagni,
      Bischeri-Strozzi,
      Strozzi-Castellani,
      Strozzi-Ridolfi,
      Castellani-Barbadori,
      Ridolfi-Tornabuori,
      Ridolfi-Medici,
      Barbadori-Medici,
      Medici-Salviati,
      Medici-Acciaiuoli,
      Medici-Tornabuori,
      Guadagni-Tornabuori,
      Salviati-Pazzi,
      Guadagni-Lamberteschi,
      Guadagni-Albizzi,
      Albizzi-Ginori,
      Medici-Albizzi,
      Pucci-Pucci, 
      simplify = FALSE) 
# graph_from_literal은 default로 simplify = TRUE가 설정되어 있음 
# self -loop가 있을 경우 변경 필요 

mat <- as.matrix(get.adjacency(g)) # igraph 개체를 행렬로 변환하기
net <- network(mat, directed=FALSE) # mat 을 network package object로 변환하기 

### 유클리드 거리 기반 구조적 등위성 산출
eq <- equiv.clust(net, method = 'euclidean', mode = 'graph')
plot(eq, labels=eq$glabels)     
### 블록모델링
b <- blockmodel(net, eq, h=3, mode = 'graph') 
plot(b, cex.axis = 0.5, cex = 0.5)      

### 상관관계 기반 블록모델링(CONCOR)
devtools::install_github('aslez/concoR')
m0 <- cor(mat)  # 상관관계 산출
round(m0, 2)   # 소수점 둘째자리까지 상관관계 살펴보기 
library(concoR)
blks <- concor_hca(list(mat), p = 2) 
blk_mod <- blockmodel(net, blks$block) 
blk_mod

plot(blk_mod, labels=blks$vertex)

### 블록모델 결과 최적화
#install.packages('blockmodeling')
library(blockmodeling)
####  Try a two block partition.
class2 <- optRandomParC(M=mat, k=2, rep=10, approach = 'ss', blocks = 'com')

#### Try a four block partition
class4 <- optRandomParC(M=mat, k=4, rep=10, approach = 'ss', blocks = 'com')
```

par(mfrow=c(1,2)) # 한 화면에 2개의 그래프가 출력되도록 layout 설정 
plot(class2, main = '') 
  title('Two Block Partition')
plot(class4, main = '') 
  title('Four Block Partition')

#### 등위집단을 구분한 후 이를 토대로 채색
V(g)$opt.blocks <- class4$best$best1$clu
plot.igraph(g, vertex.color=V(g)$opt.blocks) # plot in igraph
