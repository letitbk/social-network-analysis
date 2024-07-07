####################################
#               제 1 장             #
####################################


# 분석 환경 세팅
## package 설치
install.packages('igraph')

## package 불러들이기 
library(igraph) 

## 여러 패키지를 동시에 설치하기  
my_packages <- c('igraph', 'sna', 'network', 'dplyr', 'kable', 'kableExtra', 'readxl', 'ggplot2', 'reshape2', 'devtools', 'remotes', 'keyplayer', 'netseg', 'extrafont', 'ggrepel', 'blockmodeling', 'backbone', 'data.table', 'egor', 'migraph', 'ergm.ego')
### 각 패키지를 설치하고 불러오기
for (package_name in my_packages) {
  #### 패키지가 설치되어 있지 않으면 설치
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name)
  }
  #### 패키지 불러오기 
  library(package_name, character.only = TRUE)

## github에 공개된 개발자 소스 코드 활용하여 패키지 설치하기 
remotes::install_github(c('gastonstat/arcdiagram', 'biometry/bipartite/bipartite', 'aslez/concoR'))

### 패키지 불러오기 
library('arcdiagram')
library('bipartite')
library('concoR')
