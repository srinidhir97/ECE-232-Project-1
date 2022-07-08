install.packages("igraph")

library(igraph)

library("ggplot2")

connected = 0
for (i in seq(1:1000)){
    g<- barabasi.game(1000, m = 1,directed=FALSE)
    #print(is.connected(g))
    if (is.connected(g) == TRUE){
        print(is.connected(g))
        connected<- connected + 1
    }
}
    plot(g,vertex.size=5, vertex.label.cex=0.2)

#Such a network is always connected

g1<- barabasi.game(1000, m = 1,directed=FALSE)
fg1<- cluster_fast_greedy(g1)
print(fg1)
mod1<- modularity(fg1)
print(mod1)

plot(fg1,g1,vertex.size=5, vertex.label.cex=0.2)


g2<- barabasi.game(10000, m = 1,directed=FALSE)
fg2<- cluster_fast_greedy(g2)
mod2<- modularity(fg2)
print(mod2)
plot(fg2,g2,vertex.size=5, vertex.label.cex=0.2)

#the modularity is higher with more nodes. 

x1<- degree_distribution(g1, mode="in", cumulative=TRUE)
x2<- degree_distribution(g2, mode="in", cumulative=TRUE)

plot(x1, log="xy",type="b", main="Degree Distributions On A Log-Log Scale of network with n = 1000 nodes", xlab = "log(degree)", ylab = "log(frequency)") 
logdeg1 <- log(seq(1,length(x1[-1]))) 
logdistrib1 <- log(x1[-1])

idx1 <- which(logdistrib1 != -Inf, arr.ind=TRUE)
logdistrib1<- logdistrib1[c(idx1)]
logdeg1 <- logdeg1[c(idx1)]

lm(logdistrib1~logdeg1)

plot(x2, log="xy",type="b", main="Degree Distributions On A Log-Log Scale of network with n = 10000 nodes", xlab = "log(degree)", ylab = "log(frequency)")
logdeg2 <- log(seq(1,length(x2[-1]))) 
logdistrib2 <- log(x2[-1])

idx2 <- which(logdistrib2 != -Inf, arr.ind=TRUE)
logdistrib2<- logdistrib2[c(idx2)]
logdeg2 <- logdeg2[c(idx2)]

lm(logdistrib2~logdeg2)

g1<- barabasi.game(1000, m = 1,directed=FALSE)
g2<- barabasi.game(10000, m = 1,directed=FALSE)

degreelist <- c()
for (i in 1:1000) 
{
    node_i <- sample(vcount(g1), 1) 
    neighbors_node_i <- neighbors(g1, node_i)
    node_j <- sample(neighbors_node_i, 1)   
    degreelist <- c(degreelist, degree(g1, node_j))
}
   
data_j <- as.data.frame(table(degreelist))
log_degree_neighbors <- log(as.numeric(data_j$degreelist))
log_distribution_neighbors <- log(as.numeric(data_j$Freq))

plot(log_degree_neighbors,log_distribution_neighbors, type = "b", main = "Degree Distributions with n = 1000 nodes; RANDOM", xlab = "log(degree)", ylab = "log(frequency)")

lm(log_distribution_neighbors ~ log_degree_neighbors)


degreelist2 <- c()
for (i in 1:10000) 
{
    node_i2 <- sample(vcount(g2), 1) 
    neighbors_node_i2 <- neighbors(g2, node_i2)
    node_j2<- sample(neighbors_node_i2, 1)   
    degreelist2 <- c(degreelist2, degree(g2, node_j2))
}
   
data_j2 <- as.data.frame(table(degreelist2))
log_degree_neighbors2 <- log(as.numeric(data_j2$degreelist2))
log_distribution_neighbors2 = log(as.numeric(data_j2$Freq))

plot(log_degree_neighbors2,log_distribution_neighbors2, type = "b", main = "Degree Distributions with n = 10000 nodes; RANDOM", xlab = "log(degree)", ylab = "log(frequency)")

lm(log_distribution_neighbors2 ~ log_degree_neighbors2)

listdegree = rep(0, 1000)

for(i in 1:10000){
    g <- barabasi.game(1000, m = 1 ,directed=FALSE)
    listdegree <- listdegree + degree(g)
}
listdegree <- listdegree / 10000
plot(c(1000:1),listdegree,xlab="Aging",ylab="Degree")

#the degree increases exponentially as  the age of the nodes increases

#a
connected = 0
for (i in seq(1:1000)){
    g<- barabasi.game(1000, m = 2,directed=FALSE)
    #print(is.connected(g))
    if (is.connected(g) == TRUE){
        print(is.connected(g))
        connected<- connected + 1
    }
}
    plot(g,vertex.size=5, vertex.label.cex=0.2)


#b
g1<- barabasi.game(1000, m = 2,directed=FALSE)
fg1<- cluster_fast_greedy(g1)
print(fg1)
mod1<- modularity(fg1)
print(mod1)

plot(fg1,g1,vertex.size=5, vertex.label.cex=0.2)
#c
g2<- barabasi.game(10000, m = 2,directed=FALSE)
fg2<- cluster_fast_greedy(g2)
mod2<- modularity(fg2)
print(mod2)
plot(fg2,g2,vertex.size=5, vertex.label.cex=0.2)

#the modularity is higher with more nodes. 
#d
x1<- degree_distribution(g1, mode="in", cumulative=TRUE)
x2<- degree_distribution(g2, mode="in", cumulative=TRUE)

plot(x1, log="xy",type="b", main="Degree Distributions On A Log-Log Scale of network with n = 1000 nodes", xlab = "log(degree)", ylab = "log(frequency)") 
logdeg1 <- log(seq(1,length(x1[-1]))) 
logdistrib1 <- log(x1[-1])

idx1 <- which(logdistrib1 != -Inf, arr.ind=TRUE)
logdistrib1<- logdistrib1[c(idx1)]
logdeg1 <- logdeg1[c(idx1)]

lm(logdistrib1~logdeg1)

plot(x2, log="xy",type="b", main="Degree Distributions On A Log-Log Scale of network with n = 10000 nodes", xlab = "log(degree)", ylab = "log(frequency)")
logdeg2 <- log(seq(1,length(x2[-1]))) 
logdistrib2 <- log(x2[-1])

idx2 <- which(logdistrib2 != -Inf, arr.ind=TRUE)
logdistrib2<- logdistrib2[c(idx2)]
logdeg2 <- logdeg2[c(idx2)]

lm(logdistrib2~logdeg2)
#e
g1<- barabasi.game(1000, m = 2,directed=FALSE)
g2<- barabasi.game(10000, m = 2,directed=FALSE)

degreelist <- c()
for (i in 1:1000) 
{
    node_i <- sample(vcount(g1), 1) 
    neighbors_node_i <- neighbors(g1, node_i)
    node_j <- sample(neighbors_node_i, 1)   
    degreelist <- c(degreelist, degree(g1, node_j))
}
   
data_j <- as.data.frame(table(degreelist))
log_degree_neighbors <- log(as.numeric(data_j$degreelist))
log_distribution_neighbors <- log(as.numeric(data_j$Freq))

plot(log_degree_neighbors,log_distribution_neighbors, type = "b", main = "Degree Distributions with n = 1000 nodes; RANDOM", xlab = "log(degree)", ylab = "log(frequency)")

lm(log_distribution_neighbors ~ log_degree_neighbors)


degreelist2 <- c()
for (i in 1:10000) 
{
    node_i2 <- sample(vcount(g2), 1) 
    neighbors_node_i2 <- neighbors(g2, node_i2)
    node_j2<- sample(neighbors_node_i2, 1)   
    degreelist2 <- c(degreelist2, degree(g2, node_j2))
}
   
data_j2 <- as.data.frame(table(degreelist2))
log_degree_neighbors2 <- log(as.numeric(data_j2$degreelist2))
log_distribution_neighbors2 = log(as.numeric(data_j2$Freq))

plot(log_degree_neighbors2,log_distribution_neighbors2, type = "b", main = "Degree Distributions with n = 10000 nodes; RANDOM", xlab = "log(degree)", ylab = "log(frequency)")

lm(log_distribution_neighbors2 ~ log_degree_neighbors2)
#f
listdegree = rep(0, 1000)

for(i in 1:10000){
    g <- barabasi.game(1000, m = 2 ,directed=FALSE)
    listdegree <- listdegree + degree(g)
}
listdegree <- listdegree / 10000
plot(c(1000:1),listdegree,xlab="Aging",ylab="Degree")

#the degree increases exponentially as  the age of the nodes increases

#a
connected = 0
for (i in seq(1:1000)){
    g<- barabasi.game(1000, m = 5,directed=FALSE)
    #print(is.connected(g))
    if (is.connected(g) == TRUE){
        print(is.connected(g))
        connected<- connected + 1
    }
}
    plot(g,vertex.size=5, vertex.label.cex=0.2)


#b
g1<- barabasi.game(1000, m = 5,directed=FALSE)
fg1<- cluster_fast_greedy(g1)
print(fg1)
mod1<- modularity(fg1)
print(mod1)

plot(fg1,g1,vertex.size=5, vertex.label.cex=0.2)
#c
g2<- barabasi.game(10000, m = 5,directed=FALSE)
fg2<- cluster_fast_greedy(g2)
mod2<- modularity(fg2)
print(mod2)
plot(fg2,g2,vertex.size=5, vertex.label.cex=0.2)

#the modularity is higher with more nodes. 
#d
x1<- degree_distribution(g1, mode="in", cumulative=TRUE)
x2<- degree_distribution(g2, mode="in", cumulative=TRUE)

plot(x1, log="xy",type="b", main="Degree Distributions On A Log-Log Scale of network with n = 1000 nodes", xlab = "log(degree)", ylab = "log(frequency)") 
logdeg1 <- log(seq(1,length(x1[-1]))) 
logdistrib1 <- log(x1[-1])

idx1 <- which(logdistrib1 != -Inf, arr.ind=TRUE)
logdistrib1<- logdistrib1[c(idx1)]
logdeg1 <- logdeg1[c(idx1)]

lm(logdistrib1~logdeg1)

plot(x2, log="xy",type="b", main="Degree Distributions On A Log-Log Scale of network with n = 10000 nodes", xlab = "log(degree)", ylab = "log(frequency)")
logdeg2 <- log(seq(1,length(x2[-1]))) 
logdistrib2 <- log(x2[-1])

idx2 <- which(logdistrib2 != -Inf, arr.ind=TRUE)
logdistrib2<- logdistrib2[c(idx2)]
logdeg2 <- logdeg2[c(idx2)]

lm(logdistrib2~logdeg2)
#e
g1<- barabasi.game(1000, m = 5,directed=FALSE)
g2<- barabasi.game(10000, m = 5,directed=FALSE)

degreelist <- c()
for (i in 1:1000) 
{
    node_i <- sample(vcount(g1), 1) 
    neighbors_node_i <- neighbors(g1, node_i)
    node_j <- sample(neighbors_node_i, 1)   
    degreelist <- c(degreelist, degree(g1, node_j))
}
   
data_j <- as.data.frame(table(degreelist))
log_degree_neighbors <- log(as.numeric(data_j$degreelist))
log_distribution_neighbors <- log(as.numeric(data_j$Freq))

plot(log_degree_neighbors,log_distribution_neighbors, type = "b", main = "Degree Distributions with n = 1000 nodes; RANDOM", xlab = "log(degree)", ylab = "log(frequency)")

lm(log_distribution_neighbors ~ log_degree_neighbors)


degreelist2 <- c()
for (i in 1:10000) 
{
    node_i2 <- sample(vcount(g2), 1) 
    neighbors_node_i2 <- neighbors(g2, node_i2)
    node_j2<- sample(neighbors_node_i2, 1)   
    degreelist2 <- c(degreelist2, degree(g2, node_j2))
}
   
data_j2 <- as.data.frame(table(degreelist2))
log_degree_neighbors2 <- log(as.numeric(data_j2$degreelist2))
log_distribution_neighbors2 = log(as.numeric(data_j2$Freq))

plot(log_degree_neighbors2,log_distribution_neighbors2, type = "b", main = "Degree Distributions with n = 10000 nodes; RANDOM", xlab = "log(degree)", ylab = "log(frequency)")

lm(log_distribution_neighbors2 ~ log_degree_neighbors2)
#f
listdegree = rep(0, 1000)

for(i in 1:10000){
    g <- barabasi.game(1000, m = 5 ,directed=FALSE)
    listdegree <- listdegree + degree(g)
}
listdegree <- listdegree / 10000
plot(c(1000:1),listdegree,xlab="Aging",ylab="Degree")

#the degree increases exponentially as  the age of the nodes increases

g <- barabasi.game(1000, m = 1 ,directed=FALSE)
degree_seq <- degree(g)

graph2 <- sample_degseq(degree_seq, method="simple.no.multiple") 
fg <- cluster_fast_greedy(g)
cmsize <- sizes(fg)
degree_seqgraph2 = degree(graph2)
print(modularity(fg))


plot(fg,g,vertex.size=3, vertex.label.cex=0.2)
fgraph2 <- cluster_fast_greedy(graph2)
cmsizegraph2 <- sizes(fgraph2)
print(modularity(fgraph2))
plot(fgraph2,graph2,vertex.size=3, vertex.label.cex=0.2)

#first procedure, they're connected, meanwhile in the second they are not. 

#a
g <-sample_pa_age(n=1000, m=1, pa.exp=1, aging.exp=-1, directed=FALSE)
x <-degree.distribution(g)
plot(x, log ="xy", type = 'b', main= "Degree distribution for nodes,m n=1000", xlab="log(degree)", ylab=" log(frequency)")

logdeg <- log(seq(1,length(x[-1]))) 
logdistrib <- log(x[-1])

idx <- which( logdistrib != -Inf, arr.ind=TRUE)
logdistrib <- logdistrib[c(idx)]
logdeg <- logdeg[c(idx)]

lm(logdistrib~logdeg)



#1.3.b
fg <- cluster_fast_greedy(g)
mod <- modularity(fg)
print(mod)
plot(fg1,g1,vertex.size=5, vertex.label.cex=0.2)
