############   第二章  R语言编程基础2  ###########



#########################################################
#####  Section 2.1： 数据管理   ######


### 1.变量重命名(***)：names()，colnames()
    ## 1) fix()函数：交互式编辑器(*)
(score <- data.frame(student = c("A", "B", "C", "D"), 
                     gender = c("M", "M",  "F", "F"), 
                     math = c(90, 70, 80, 60), 
                     Eng = c(88, 78, 69, 98), 
                     p1 = c(66, 59, NA, 88)))

fix(score)    
score.list <- as.list(score)      
fix(score.list) 




    ## 2) rename()函数：可修改数据框或列表的变量名，但不能修改矩阵的变量名
library(reshape)       
rename(score, c(p1 = "Chinese"))  
rename(score.list, c(p1 = "Chinese"))  




    ## 3) names()函数(***)：可修改数据框或列表的变量名，但不能修改矩阵的变量名
names(score)[5] <- "Chinese"  
score



    ## 4) colnames()函数(**)：可修改数据框、列表、及矩阵的变量名
colnames(score)[5] <- "Chinese"  
rownames(score) <- letters[1:4]  
score









### 2. 缺失、重复与异常值的清洗
    ## 1) 缺失值的发现与清洗
is.na(score)  
anyNA(score)  
na.omit(score)  

complete.cases(score)  
which(complete.cases(score)==F)   
score[complete.cases(score), ]  



saledata <- read.csv(file = "catering_sale.csv", header = TRUE) 
head(saledata)
names(saledata) <- c("date", "sale")
head(saledata)
sum(complete.cases(saledata))        
sum(!complete.cases(saledata))       
mean(!complete.cases(saledata))
saledata[!complete.cases(saledata), ]





    ## 2) boxplot()：异常值检测
(sp <- boxplot(saledata$sale, boxwex = 0.7))
sp$out
which(saledata$sale == 6607.4)
saledata$sale[9] = NA      

title("销量异常值检测箱线图")
xi <- 1.1
sd.s <- sd(saledata[complete.cases(saledata), ]$sale)        
mn.s <- mean(saledata[complete.cases(saledata), ]$sale)      
points(xi, mn.s, col = "red", pch = 18)
arrows(xi, mn.s - 1.645*sd.s, xi, mn.s + 1.645*sd.s, code = 3, col = "pink", angle = 75, length = .1)
text(rep(c(1.05, 1.05, 0.95, 0.95), length = length(sp$out)), 
     sp$out[order(sp$out)] + rep(c(150, -150, 150, -150), length = length(sp$out)), 
     labels = sp$out[order(sp$out)], col = "red")
    





    ## 3) 重复值分析：duplicated(), unique()
(x = rbind(5:10, runif(6), runif(6), 5:10, 11:16))
(y = sample(1:20,20, rep=T))
duplicated(x)
duplicated(y)
x[!duplicated(x), ]      
unique(x)      

y[!duplicated(y)]
unique(y)










### 3. 数据转换、排序、抽样与概率分布
    ## 1) 数据转换
    ## a) 简单函数变换：对数log()，指数exp()，差分diff()，累加cumsum()等



    ## b) 规范化**
data <- read.csv('normalization.csv', header = FALSE)
head(data)

    ### b1) 最小-最大规范化：极差标准化*
b1 <- (data[, 1] - min(data[, 1])) / (max(data[, 1]) - min(data[, 1]))
b2 <- (data[, 2] - min(data[, 2])) / (max(data[, 2]) - min(data[, 2]))
b3 <- (data[, 3] - min(data[, 3])) / (max(data[, 3]) - min(data[, 3]))
b4 <- (data[, 4] - min(data[, 4])) / (max(data[, 4]) - min(data[, 4]))
data_scatter <- cbind(b1, b2, b3, b4)

    ### b2) 均值-标准差规范化：标准差标准化**
data_zscore <- scale(data)

    ### b3) 小数定标规范化：映射到[-1, 1]区间 (略)
i1 <- ceiling(log(max(abs(data[, 1])), 10))  
c1 <- data[, 1] / 10 ^ i1
i2 <- ceiling(log(max(abs(data[, 2])), 10))
c2 <- data[, 2] / 10 ^ i2
i3 <- ceiling(log(max(abs(data[, 3])), 10))
c3 <- data[, 3] / 10 ^ i3
i4 <- ceiling(log(max(abs(data[, 4])), 10))
c4 <- data[, 4] / 10 ^ i4
data_dot <- cbind(c1, c2, c3, c4)


    ### 打印结果
options(digits = 4)    # 控制输出结果的有效位数
data
data_scatter
data_zscore
data_dot





    ## 2) 数据排序(***)：order()
    ## sort()函数
sort(score$math)                       
sort(score$math, decreasing = TRUE)    
sort(score$Chinese, na.last = TRUE) 


    ## rank()函数
x <- c(3, 4, 2, 5, 5, 3, 8, 11)
rank(x)      




    ### order()函数：既可对向量排序，又可对“数据框排序”(***)
order(score$math)  
score$math[order(score$math)]
score[order(score$math), ]        
score[order(-score$math), ]  
score[order(score$math, score$Eng), ] 





    ## 3) 随机抽样：sample()
    ## a) sample()函数(***)：简单随机抽样，也可“随机分组”
sample(1:100, 20)    
sample(1:100, 20, rep=T)    
set.seed(12345)    ;   sample(1:1000,15)     
sample(1:1000,15) 
sample(1:100, 20, prob=1:100)    


LETTERS
sample(LETTERS, 5, replace=TRUE)     
sample(LETTERS, 5, replace=FALSE)   

    ### 随机分组(***)：将26个元素(此处为字母)、分为2组，第1组和第2组比例为7:3
(n <- sample(2, 26, replace = T, prob = c(0.7, 0.3)))    
length(n)  ;   class(n)  ;  mode(n)  ;   typeof(n)
(sample1 <- LETTERS[n == 1])         
(sample2 <- LETTERS[n == 2])         

x <- -499:500         ;   length(x)
(n <- sample(4, length(x), replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.4))) 
(x1 <- x[n == 1])     ;   length(x1)      
(x2 <- x[n == 2])     ;   length(x2)     
(x3 <- x[n == 3])     ;   length(x3)     
(x4 <- x[n == 4])     ;   length(x4)     



    ## b). srswr()函数：Simple random sampling with replacement(略)


    ## c) srswor()函数：无重复的简单随机抽样(略)





    ## 4) 概率统计：数据的分布、统计、算术、累积运算(***)
(x <- round(runif(20,1,1000), digits = 2))     

summary(x)    
min(x)  ;  max(x)        
mean(x) ; median(x)      
var(x)        
sd(x) ; sqrt(var(x))     
fivenum(x)    
quantile(x)   
quantile(x,c(0, .33, .66, 1))      
mad(x)       
cor(x, sin(x/20))        

round(x)      
signif(x, digits = 3)	   
floor(x)     
ceiling(x)    

cumsum(x)     
cummax(x)  ; cummin(x)     
prod(x)
cumprod(x)   














### 4. 字符串处理(***)：查询、替换、连接，grep()、sub()、paste()
    ## 1) 正则表达式(*)
# 正则表达式，即用于“描述与匹配、一个文本集合的表达式”
# 元字符，不用来描述它自身、而被“转义”了，如表示重复操作的元字符 ( {}、*、+、? )
# 运算顺序：圆括号 > 表示重复次数的操作 > 连接运算  > 可选项运算(|) 



    ## 2) 字符串查询(***)：grep()、substr()
txt <- c("Whatever", "is", "worth", "doing", "is", "worth", "doing", "well")
nchar(txt)        
grep("e.*r|wo", txt, fixed = FALSE)  
grepl("e.*r|wo", txt)  
(sentence <- paste("Whatever", "is", "worth", "doing", "is", "worth", "doing", "well", sep=" "))
substr(sentence, 10, 23)


gregexpr("e.*r|wo", txt)        # (略)
regexec("e.*r|wo", txt)         # (略)
regexpr("e.*r|wo", txt)         # (略)


    ## 3) 字符串替换(***)：sub()，gsub()
sub("[tr]", "k", txt)  
gsub("[tr]", "k", txt)  


    ## 4) 字符串拆分：strsplit()
data <- c("2016年1月1日", "2016年2月1日")
strsplit(data, "年")  
strsplit(data, "年")[[1]][1]    
    

    ## 5) 字符串连接(***)：paste()
paste("xy", 1:5, sep = "")
paste("xy", 1:5, sep = "", collapse = ";")
cat("xy", 1:5, sep = "")
    

    ### 利用列表，连接“两个字符向量的对应元素/字段”
x <- list(a = "1st", b = "2nd", c = "3rd")
y <- list(d = 1, e = 2)
paste(x, y, sep = "-")  
paste(x, y, sep = "-", collapse = "; ")  
paste(x, collapse = " , ")  











#########################################################
#####  Section 2.2： 控制语句与函数编写   ######


### 1. 控制语句(分支 + 循环)：“条件if/switch + 循环for/while/repeat”
    ## 1) 分支/条件语句：if, switch
    ### a) if_else 条件结构***、if条件句
a <- 0.38
if (a <= 0)  { 
    result <- 0 
}  else if (a < 1)    result <- 1   else
    result <- 2        
result

x <- -3:3
ifelse( x>0, sqrt(x), x^2)
sqrt(ifelse(x>0, x, x^4))
   

    ### 单条if语句(条件为假时，直接跳过条件句、执行其他语句)：if(cond) {expr} 其它语句
fun.algorithm <- function(x, y, method = "add")      
{
    if(method == "add")          res <- x + y     
    if(method == "substract")    res <- x - y
    if(method == "乘")           res <- x * y
    if(method == "幂运算")       res <- x^y
    return(res)                          
}                                               

    ## 算法函数的调用，结果检验
fun.algorithm(x = 10, y = 4)
fun.algorithm(x = 10, y = 4, method = "add")
fun.algorithm(x = 10, y = 4, method = "substract")
fun.algorithm(x = 10, y = 4, method = "幂运算")



    ### b) switch()：条件分支(*)
switch(2, mean(1:10), 1:5, 1:10)  
y <- "fruit"
switch(y, fruit = "apple", vegetable = "broccoli", meat = "beef")  





    ## 2) 循环语句：for, while, repeat
    ### a) for循环(***)
n <- c(2, 100, 9)
for (i in n)                           
{  x <- sqrt(i)                          
cat("sqrt(",  i,  ") ＝",  x, "\n")      
}   
    



    ### b) while循环(**)
    ### 生成30个数的费波那契数列
(x <- c(1, 2))
i <- 3                
while (i <= 30) {     
    x[i] <- x[i - 1] + x[i - 2]      
    i <- i + 1 } 
x  

    ## 采用for循环，同样实现以上算法
(x <- c(1, 2))
for(i in 3:30) {
    x[i] <- x[i - 1] + x[i - 2] 
}
x



    
    ### c) repeat-break循环：repeat {if(cond) break expr}
    ### 根据用户的点击数pv，将用户分为“初级用户”、“中级用户”或“高级用户”
pv <- c(1, 1, 2, 3, 1, 1, 15, 7, 18)
i <- 1        
result <- ""         
repeat{
    if (i > length(pv)) {         
        break
    }
    if (pv[i] <= 5) { 
        result[i] <- "初级用户"     
    } else if (pv[i] <= 15) {
        result[i] <- "中级用户"     
    } else {
        result[i] <- "高级用户"     
    }
    i <- i + 1
}
result





    ### d) 4）非向量集合的循环空间 (略)：







### 2. 函数编写(**)
    ### a) 自编函数计算标准差
sd2 <- function(x) {
    if (!is.numeric(x)) {                          
        stop("the input data must be numeric!\n") 
    }                                            
    if (length(x) == 1){                          
        stop("can not compute sd for one number, 
         a numeric vector required.\n") 
    }
    dv <- c()
    dv2 <- c()      
    for (i in 1:length(x)) {   
        dv[i] <- x[i] - mean(x)  
        dv2[i] <- dv[i] ^ 2
    }
    sum2 <- sum(dv2)    
    sd <- sqrt(sum2 / (length(x) - 1))
    return(sd)     
}


    ## 程序的检验
sd2(c(2, 6, 4, 9, 12, 3, 2))
sd2(3)                 
sd2(c("1", "2"))       





    ### b) 自定义一个“描述性统计 + 绘图”的向量函数，读取数据后、再调用函数
    ## b1) 先读取数据
library(RODBC)                                          
SSEC_Data <- odbcConnectExcel2007('stock index.xls')        
SSEC <-sqlFetch(SSEC_Data, 'SSEC')                       
SSEC_shjc <- sqlFetch(SSEC_Data, 'SSEC-600009')          
SSEC_scgf <- sqlFetch(SSEC_Data, 'SSEC-600008')            
SSEC_zggm <- sqlFetch(SSEC_Data, 'SSEC-600007')             
close(SSEC_Data)
detach(package:RODBC)



    ## b2) 调用自定义的函数、计算
    ## 方法1：直接在R-studio中运行Rfunction-Statistic.R文件中的“自编函数statmy()”，运行后、再调用
(SSEC_shjc_stat <- statmy(x=SSEC_shjc[,2], plot.it=TRUE))    
(SSEC_scgf_stat <- statmy(x=SSEC_scgf[,2], plot.it=TRUE))
(SSEC_zggm_stat <- statmy(x=SSEC_zggm[,2], plot.it=TRUE))

stats <- rbind(SSEC_shjc_stat$stats, SSEC_scgf_stat$stats, SSEC_zggm_stat$stats)
rownames(stats) <- c('shjc', 'scgf', 'zggm')           
print(stats)
tests <- rbind(SSEC_shjc_stat$JBtest, SSEC_scgf_stat$JBtest, SSEC_zggm_stat$JBtest)
rownames(tests) <- c('shjc', 'scgf', 'zggm')
print(tests)


    ## 方法2：利用source()命令，调用含“自编函数statmy()”的R文件Rfunction-Statistic.R , 即可 
source("Rfunction-Statistic.R")      











#########################################################
#####  Section 2.3： 程序调试   ######


### 1. 案例1：“自编函数statmy()”的调试
    ## 1) 利用debug()函数，打开browser浏览器、展开调试
source('Rfunction-Statistic.R')
debug(statmy)
data.sim.1 <- rnorm(1000)             
data.sim.2 <- matrix(rnorm(2000), nrow=2)
statmy(x=data.sim.1, plot.it=TRUE)
statmy(x=data.sim.2, plot.it=TRUE)
    


    ## 2) 利用browser()函数，打开browser浏览器、展开调试
rm(list=ls())
source('sub-01.R')        
set.seed(12345)
data.sim.1 <- rnorm(1000)             
data.sim.2 <- matrix(rnorm(2000), nrow=2)
statmy(x=data.sim.1, plot.it=TRUE)     
statmy(x=data.sim.2, plot.it=TRUE)     
   





### 2. 案例2：“寻找1的游程”程序的调试
    ## 注：在0和1组成的序列中，一个由连续的0或1构成的串、称为一个游程run。
    ## 1) 初步调试：发现运算优先级错误
findruns <- function(x, k) {
    n <- length(x)
    runs <- NULL
    for (i in 1:(n-k)) {
        if (all(x[i:i+k-1]==1)) runs <- c(runs, i)
    }
    return(runs)
}

x <- c(1, 0, 0, 1, 1, 0, 1, 1, 1)
findruns(x, 2)
    


    ## 运用debug()函数，进入浏览器进行调试
debug(findruns)
findruns(x, 2)



    ## 2) 
findruns <- function(x, k) {
    n <- length(x)
    runs <- NULL
    for (i in 1:(n-k)) {
        if (all(x[i:(i+k-1)]==1)) runs <- c(runs, i)
    }
    return(runs)
}

x <- c(1, 0, 0, 1, 1, 0, 1, 1, 1)
findruns(x, 2)
    # 结果依然不正确，正确结果应是 (4, 7, 8) 。


    ## 在循环里设置一个断点，进行更细致的查看：
source("findruns.R")
setBreakpoint("findruns.R", 6)
x <- c(1, 0, 0, 1, 1, 0, 1, 1, 1)
findruns(x, 2)
    


    ## 最后一次迭代时，额外再次设置一个调试断点
findruns <- function(x, k) {
    n <- length(x)
    runs <- NULL
    for (i in 1:(n-k)) {
        if (all(x[i:(i+k-1)]==1)) runs <- c(runs, i)
        if (i == n-k) browser()
    }
    return(runs)
}

x <- c(1, 0, 0, 1, 1, 0, 1, 1, 1)
findruns(x, 2)


    ## 修正循环范围后，再次验证、最后得到正确结果
findruns <- function(x, k) {
    n <- length(x)
    runs <- NULL
    for (i in 1:(n-k+1)) {
        if (all(x[i:(i+k-1)]==1)) runs <- c(runs, i)
    }
    return(runs)
}

x <- c(1, 0, 0, 1, 1, 0, 1, 1, 1)
findruns(x, 2)











#########################################################
#####  Section 2.4： 绘图   ######


              ### 2.4.3  图形要素 [色点线cpl、文例轴tla] 

### 1. 颜色
    ## 1) 固定颜色的选择(***)
    ### colors()函数：生成657种颜色名称
(col <- colors())       
colors()[1:30]          


    ### 绘图参数col(***)：可改变图像、坐标轴、文本、点、线等的颜色
par(mfrow = c(length(colors()) %/% 60 + 1, 1))
par(mar = c(0.1, 0.1, 0.1, 0.1), xaxs = "i",  yaxs = "i")
for (i in 1:(length(colors()) %/% 60 + 1)) 
{  barplot(rep(1, 60), col = colors()[((i - 1) * 60 + 1):(i * 60)], 
           border = colors()[((i - 1) * 60 + 1):(i * 60)], axes = FALSE)
    box() 
}
par(mfrow = c(1, 1), mar=c(2, 2, 1, 1))
   


    ### palette()函数
palette()               
palette(colors()[1:10])  
palette()  
palette("default")      
palette()
    


data(mtcars)  
head(mtcars)  ; tail(mtcars)  ; dim(mtcars) 
?mtcars     
op <- par(mfrow = c(1, 1), bg = "yellow")
plot(mtcars$wt, mtcars$mpg, col = "blue") 
par(op)     
plot(mtcars$wt, mtcars$mpg, col = 4)  
    

attach(mtcars)    
str(mtcars)   
plot(wt, mpg, col = "red", xlim = c(1.3, 5.6), ylim = c(8, 35))
points(wt[cyl == 6], mpg[cyl == 6], col = "green")
points(wt[cyl == 8], mpg[cyl == 8], col = "blue")
legend(5, 35, legend=c(4, 6, 8), pch=1, col=c("red", "green", "blue"), bty="n")




    ## 2) 渐变颜色的生成 (略讲) 
par(mfrow = c(6, 1))
par(mar = c(0.1, 0.1, 2, 0.1), xaxs = "i", yaxs = "i")
rgb <- rgb(red = 255, green = 1:255, blue = 0, alpha=100, max = 255)
barplot(rep(1, 255), col =  rgb, border = rgb, axes = FALSE, main = "rgb"); box()
barplot(rep(1, 100), col = rainbow(100), border = rainbow(100), axes = FALSE, 
        main = "rainbow(100))"); box()
barplot(rep(1, 100), col = heat.colors(100), border = heat.colors(100), 
        axes = FALSE, main = "heat.colors(100))"); box()
barplot(rep(1, 100), col = terrain.colors(100), border = terrain.colors(100), 
        axes = FALSE, main = "terrain.colors(100))"); box()
barplot(rep(1, 100), col = topo.colors(100), border = topo.colors(100), 
        axes = FALSE, main = "topo.colors(100))"); box()
barplot(rep(1, 100), col = cm.colors(100), border = cm.colors(100), axes = FALSE, 
        main = "cm.colors(100))"); box()



    ### RColorBrewer包：连续、极端、离散，三套调色板“配色方案” 
library(RColorBrewer)
par(mar = c(0.1, 3, 0.1, 0.1), mfrow = c(1, 1))
display.brewer.all(type = "seq")　　　　
display.brewer.all(type = "div")　　　　
display.brewer.all(type = "qual")　　　 



    ## 渐变色生成函数实例
library(RColorBrewer)
attach(mtcars) 
(cl <- brewer.pal(3, "Dark2"))  　　
par(mfrow = c(1, 1))
plot(wt, mpg, col = cl[1])
points(wt[cyl == 6], mpg[cyl == 6], col = cl[2])
points(wt[cyl == 8], mpg[cyl == 8], col = cl[3])
legend(5, 35, c(4, 6, 8), pch = 1, col = cl, bty = "n")

(cl <- rainbow(3))
plot(wt, mpg, col = cl[1])
points(wt[cyl == 6], mpg[cyl == 6], col = cl[2])
points(wt[cyl == 8], mpg[cyl == 8], col = cl[3])
legend(5, 35, c(4, 6, 8), pch = 1, col = cl, bty = "n")







### 2. 点(*)：points()
par(mfrow = c(1, 1), mar=c(2, 2, 1, 1))
plot(1, col = "white", xlim = c(0.9, 8), ylim = c(1, 7))
symbol <- c("*", "、", ".", "o", "O", "0", " + ", " - ", "|")


    ### 创建循环，依不同条件、添加35种不同的点(***)
for (i in 0:34) {
    x <- (i %/% 5) * 1 + 1
    y <- 6 - (i %% 5)
    if (i > 25) {
        points(x, y, pch = symbol[i - 25], cex = 1.3)
        text(x + 0.5, y + 0.1, labels = paste("pch = ", symbol[i - 25]), cex = 0.8)
    }  else  {
        if (sum(21:25 == i) > 0) { 
            points(x, y, pch = i, bg = "red", cex = 1.3)
        } else {
            points(x, y, pch = i)
        }
        text(x + 0.5, y + 0.1, labels = paste("pch = ", i), cex = 0.8)
    }
}



    ### 依定性因子条件、改变点的样式：pch参数，取值0~25
attach(mtcars)                 
cyl <- as.factor(cyl)
plot(wt, mpg, col = "white")   
points(wt, mpg, pch = as.integer(cyl) + 1, col = as.integer(cyl) + 1)
legend(5, 35, c(4, 6, 8), pch = 2:4, col = 2:4, bty = "n")

plot(wt, mpg, pch = as.integer(cyl) + 1, col = as.integer(cyl) + 1)
legend(5, 35, c(4, 6, 8), pch = 2:4, col = 2:4, bty = "n")

detach(mtcars)                







### 3. 线(***)
    ## 1) lines()：添加线条，包括“曲线、直线” (***)
(data <- matrix(rep(1:7, 10), nrow = 7, ncol = 10))
par(mfrow = c(1, 1), mar=c(2, 2, 1, 1))
plot(data[1, ], type = "l", lty = 0, ylim = c(0, 7.5), xlim = c(-1, 10), axes = F)
text(0, 1, labels = "lty = 0")
for (i in c(2:7)) {
    lines(data[i, ], lty = i - 1)
    text(0, i, labels = paste("lty = ", i - 1))
}

(data <- matrix(rep(1:6, 10), nrow = 6, ncol = 10))
plot(data[1, ], type = "l", lwd = 0.5, ylim = c(0, 8), xlim = c(-1, 10), axes = F)
text(0, 1, labels = "lwd = 0.5")
lines(data[2, ], type = "l", lwd = 0.8)  ;  text(0, 2, labels = "lwd = 0.8")
lines(data[3, ], type = "l", lwd = 1)    ;  text(0, 3, labels = "lwd = 1")
lines(data[4, ], type = "l", lwd = 1.5)  ;  text(0, 4, labels = "lwd = 1.5")
lines(data[5, ], type = "l", lwd = 2)    ;  text(0, 5, labels = "lwd = 2")
lines(data[6, ], type = "l", lwd = 4)    ;  text(0, 6, labels = "lwd = 4")
box()



    ## 2) abline()：添加参考直线(**)
plot(c(0:10), col = "white", xlim=c(0, 10))
abline(h = c(2, 6, 8))     
abline(v = seq(2, 10, 2), lty = 2, col = "blue")
abline(a = 1, b = 0.5)




    ## 3) segments(), arrows()：线段和箭头(*)
plot(c(0:10), col = "white", xlim=c(0, 10))
segments(2, 1, 4, 8)
arrows(4, 0, 7, 3, angle = 30)
arrows(4, 2, 7, 5, angle=60, length=0.15, code=3, lty=2) 



    ## 4) grid()：网格线
plot(c(0:10), col = "white")      
grid(nx = 4, ny = 8, lty = 2, lwd = 1, col = "blue")  



    ## 5) rug()：坐标轴须的分布
set.seed(123)         
x <- rnorm(500)        
plot(density(x))       
rug(x , ticksize = -0.03, col = "blue")  



    ### 实例：以mtcars数据集为例，查看不同的“线元素函数”的用法
attach(mtcars)  
smpg <- (mpg - min(mpg)) / (max(mpg) - min(mpg))

plot(wt, smpg, ylab = "standardized mpg")
lines(density(wt), col = "red")
arrows(1.8, 0.05, 1.5, 0.13, angle = 10, cex = 0.5)
text(2, 0.05, "核密度曲线", cex = 0.6)

abline(lm(smpg ~ wt), lty = 2, col = "green")
arrows(2, 0.5, 2, 0.7, angle = 10, cex = 0.5)
text(2, 0.45, "回归线", cex = 0.6)

segments(min(wt), max(smpg), max(wt), min(smpg), lty = 3, col = "blue")
arrows(3, 0.8, 2.5, 0.76, angle = 10, cex = 0.5)
text(3.3, 0.8, "最大最小值线段", cex = 0.6)
grid(nx = 4, ny = 5, lty = 2, col = "grey")




    ### 设置绘图参数，也可也添加线条
par(mfrow = c(1, 3))
plot(density(wt), col = "red")   
plot(wt, fitted(lm(smpg ~ wt)), type = "l", lty = 2, col = "green") 
plot(seq(min(wt), max(wt), length = 100), seq(max(smpg), min(smpg), length = 100), 
     type = "l", lty = 3, col = "blue")  








### 4. 文本
    ## 1) 添加标题：title()
plot(c(0:5), col = "white", xlab = "", ylab = "")
title(main = list("主标题", cex=1.5, font=3), sub = list("副标题", cex =1.2),
      xlab = "x轴标题", ylab = "y轴标题")



    ## 2) 任意位置，添加文本(*)：text()
plot(c(0:5), col = "white")
text(2, 4, labels = "font = 1:正常字体（默认）", font = 1)
text(3, 3, labels = "font = 2:粗体字体", font = 2)
text(4, 2, labels = "font = 3:斜体字体", font = 3)
text(5, 1, labels = "font = 4:粗斜体字体", font = 4)

plot(c(0:6), col = "white", xlim = c(1, 8))
text(2, 5, labels = "cex = 0.5:放大0.5倍", cex = 0.5)
text(3, 4, labels = "cex = 0.8:放大0.8倍", cex = 0.8)
text(4, 3, labels = "cex = 1(默认):正常大小", cex = 1)
text(5, 2, labels = "cex = 1.2:放大1.2倍", cex = 1.2)
text(6, 1, labels = "cex = 1.5:放大1.5倍", cex = 1.5)



    ## 3) 图形周边，添加文本：mtext()
plot(c(0:5), col = "white")
mtext("side = 1:下边", side = 1, line = 2)
mtext("side = 2:左边" , side = 2, line = 2)
mtext("side = 3:上边", side = 3)
mtext("side = 4:右边" , side = 4) 



    ## 4) 实例：将散点图，变为文本字符
attach(mtcars)
cyl <- as.factor(cyl)
plot(wt, mpg, col = "white", xlab = "", ylab = "")
text(wt, mpg, cyl, col = as.integer(cyl) + 1)
title(main = list("Miles per Gallon vs. Weight by Cylinder", cex = 1.5), 
      xlab = "Weight", ylab = "Miles per Gallon")

plot(wt, mpg, pch = as.character(cyl), col = as.integer(cyl) + 1, 
     xlab = "Weight", ylab = "Miles per Gallon ", 
     main = "Miles per Gallon vs. Weight by Cylinder", cex.main = 1.5)








### 5. 图例
local <- c("bottomright", "bottom", "bottomleft", "left", "topleft", "top", 
           "topright", "right", "center")
par(mar = c(4, 2, 4, 2), pty = 'm')
plot(c(0:10), col = "white")
legend(3, 8, "图例在(3, 8)", cex = 0.6)
legend(1, 13, "图例在(11, 11)", xpd = T, cex = 0.6)
for (i in 1:9) {
    legend(local[i], legend = paste("图例在", local[i]), cex = 0.6)
}
    








### 6. 坐标轴：范围、刻度标签(***)
plot(c(1:12), col = "white", xaxt = "n", yaxt = "n", ann = FALSE)

    ### axis()：不用R默认的，创建“自定义的坐标轴” (***)
axis(side=1, at = 1:12, col.axis = "green3", tick=F,
     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",  "Aug", 
                "Sep", "Oct", "Nov", "Dec"))
axis(2, at = seq(1, 12, length = 10), col.axis = "red", labels = 1:10, las = 2)
axis(3, at = seq(1, 12, length = 7), col.axis = "blue", cex.axis = 0.7, las = 2,
     tck = -.01, labels = c("Mon", "Tues", "Wed", "Thu", "Fri", "Sat", "Sun")) 
axis(4, at = seq(1, 12, length = 10), col.axis = "blue", cex.axis = 0.7, 
     tck = .03, col.ticks = "red", labels=round(seq(0, 1, length=10), 1), las=0)











             ### 2.4.4  图形组合与保存
       

### 1. 图形组合
    ## 1) par()：设置大多数绘图的“全局参数”，mfrow/mfcol, mar/mai, oma, mgp 
    ### 参数“mfrow, mfcol”：均衡的分隔“多个页面”
mfrow1 <- par(mfrow = c(2, 3))
for (i in 1:6) {
    plot(c(1:i), main = paste("I'm image:", i))
}

mar1 <- par(mar = c(4, 5, 2, 3))
for (i in 1:6) {
    plot(c(1:i), main = paste("I'm image:", i))
}
par(mar1)          

oma1 <- par(oma = c(4, 5, 2, 3))
for (i in 1:6) {
    plot(c(1:i), main = paste("I'm image:", i))
}
par(oma1)          

mgp1 <- par(mgp = c(1, 2, 3))
for (i in 1:6) {
    plot(c(1:i), main = paste("I'm image:", i))
}
par(mgp1)         
par(mfrow1)        





    ## 2) layout()：“不均衡的”分隔页面
(mat <- matrix(c(1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 6), nrow = 2, byrow = TRUE))
layout(mat)
for (i in 1:6) {
    plot(c(1:i), main = paste("I'm image:", i))
}







### 2. 图形保存
    ## 1) 输出到屏幕
windows()     
attach(mtcars)
plot(wt, mpg)

X11()         
plot(wt, mpg)

 
   
    ## 2) 输出到文件：pdf()函数
pdf("p.pdf")
plot(wt, mpg)    
dev.off()










        ### 2.4.5  绘图大案例


### 1. plot “泛型”类函数的演示**
set.seed(12345)                  
x <- sample(c(1:100),100)      
y <- sample(1:100,100) 
xt <- ts(x)                    
xy <- cbind(x, y)              
class(xy)
xy1 <- rbind(x,y)
f <- as.factor(c(rep('A',20),rep('B',30),rep('C',50)))
f
par(mfcol=c(2,4),mar=c(5,4,3,2))         
plot(x)                
plot(xt)               
plot(xy)               
plot(x,y)              
plot(y~x)              
plot(f)                
plot(table(f)/length(f))      
plot(f,y)              
 





### 2. pairs and coplot function demo (略)
data(morley)
morley
pairs(morley)   


library(car) ; some(CO2)
CO2
coplot(uptake ~ conc | Plant, data = CO2, show.given = FALSE, type = "b")   






### 3. 绘图大案例：上证指数及上市个股的对数收益率分布特征***
    ## 1) 读取Excel数据 
library(RODBC)              
SSEC_Data <- odbcConnectExcel2007('stock index.xls')              
SSEC <-sqlFetch(SSEC_Data, 'SSEC')                   
class(SSEC)   
SSEC_shjc <- sqlFetch(SSEC_Data, 'SSEC-600009')        
SSEC_scgf <- sqlFetch(SSEC_Data, 'SSEC-600008')           
SSEC_zggm <- sqlFetch(SSEC_Data, 'SSEC-600007')          
close(SSEC_Data)            
detach(package:RODBC)       



    ## 2) 计算对数收益序列
head(SSEC)
Close.ptd.SSEC <- SSEC$SSEC_Close                      
Close.rtd.SSEC <- diff(log(Close.ptd.SSEC))*100        



    ## 3) 从各自个股的数据框中、提取相应的收盘价  
Close.ptd.shjc <- SSEC_shjc$SSEC_600009_Close                        
Close.ptd.scgf <- SSEC_scgf$SSEC_600008_Close  
Close.ptd.zggm <- SSEC_zggm$SSEC_600007_Close



    ## 4) 绘制上证指数收益时间序列图、散点图、自相关图与偏自相关图
par(mfrow=c(2,2),mar=c(5,4,3,2))         
which(SSEC$SSEC_Date=="2010-12-31")      
Close.ptd.SSEC.ts<-ts(Close.ptd.SSEC,start=c(2010,1,4),freq=242)  
plot(Close.ptd.SSEC.ts, type="l",main="(a) 上证指数日收盘价序列图",
     xlab="Date",ylab="Price",cex.main=0.95,las=1)     

plot(Close.ptd.shjc[1:20], type="p",pch=17,main="(b) 上证指数样本股散点图",
     xlab="Time",ylab="Price",cex.main=0.95,ylim=c(4,14),las=1)      
points(Close.ptd.scgf[1:20],pch=15)                                 
points(Close.ptd.zggm[1:20],pch=14)                           
legend("bottomright", legend=c("SHJC_600009","SCGF_600008","ZGGM_600007"),
       pch=c(17,15,14),cex=0.6,lty=c(-1,-1,-1))                      
    
acf(Close.rtd.SSEC,main='',xlab='Lag',ylab='ACF',las=1)    
title(main='(c) 上证指数收益率自相关检验',cex.main=0.95)

pacf(Close.rtd.SSEC,main='',xlab='Lag',ylab='PACF',las=1)               
title(main='(d) 上证指数收益率偏自相关检验',cex.main=0.95)



    ## 5)  Q-Q图、 经验累积分布ecdf图、 密度图、直方图 
par(mfrow=c(2,2),mar=c(5,4,3,2)) 

qqnorm(Close.rtd.SSEC,main="(a) 上证指数收益率Q-Q图",cex.main=0.95,
       xlab='理论分位数',ylab='样本分位数')            
qqline(Close.rtd.SSEC)                                 

ECD.SSEC <- ecdf(Close.rtd.SSEC[1:10])                 
plot(ECD.SSEC,lwd = 2,main="(b) 上证指数收益率累积分布函数图",cex.main=0.95,las=1) 
xx <- unique(sort(c(seq(-3, 2, length=24), knots(ECD.SSEC))))         
lines(xx, ECD.SSEC(xx))                      
abline(v = knots(ECD.SSEC), lty=2, col='gray70')                           
x1 <- c((-4):3)             # 设定区间范围
lines(x1,pnorm(x1,mean(Close.rtd.SSEC[1:10]),sd(Close.rtd.SSEC[1:10])))  


D <-density(Close.rtd.SSEC)                          
plot(D, main="(c) 上证指数核密度曲线图 ",xlab="收益", ylab='密度',
     xlim = c(-7,7), ylim=c(0,0.5),cex.main=0.95)       
polygon(D, col="gray", border="black")                 
curve(dnorm,lty = 2, add = TRUE)                        

x2 <- c(-7:7)
lines(x2,dnorm(x2,mean=0,sd=1))      
abline(v=0,lty = 3)                                     
legend("topright", legend=c("核密度","正态密度"),lty=c(1,2),cex=0.5)


hist(Close.rtd.SSEC[1:100],xaxt='n',main='(d) 上证指数收益率直方图',
     xlab='收益/100',ylab='密度', freq=F,cex.main=0.95,las=1)        
summary(Close.rtd.SSEC[1:100])   
x2 <- seq(-6, 4, by=0.01)
lines(x2,dnorm(x2,mean(Close.rtd.SSEC[1:100]),sd(Close.rtd.SSEC[1:100]))) 
axis(1,at=axTicks(1),labels = as.integer(axTicks(1))/100 )         


