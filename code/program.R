
# Load R package ----------------------------------------------------------
library(igraph)
library(vegan)
library(here)

# function to generate an adjacency matrix ----------------------------------------

# Comment -----------------------------------------------------------------
# This function generates an adjacency matrix, i.e, this function changes the weight of an edge by 1. 
# The input is a square matrix with headers. For this reason, the index j takes values from two to dim(salida)[2]

generamosadyacencia<-function(archivo)  
{
  salida=read.table(here("data",archivo), header=TRUE, sep=",") 
  n=dim(salida)[1]
  n2=dim(salida)[2]
  matrizdeceros=matrix(rep(0),n,n)
  for( j in  2:n2)
  {
    for( i in 1:n)
    {
      if (salida[i,j]!=0)
      {
        matrizdeceros[i,j-1]=1 
      }
    }
  }
  # 
   long=length(names(salida))
   c1=names(salida)[2:long]  
   colnames(matrizdeceros)=c1 #names
   rownames(matrizdeceros)=c1 #names
  
  write.table(matrizdeceros,file="data/matrizady.csv")   
}


# This function identifies the region where we make the changes --------------------------------

region<-function(archivo)  
{
  salida=read.table(here("data",archivo), header=TRUE, sep=" ") 
  n=dim(salida)[1]
  n2=dim(salida)[2]
  filas=c()
  columnas=c()
  contador=0
  for( i in  1:n) #rows
  {
    for( j in 1:n2) #columns
    {
      if (salida[i,j]!=0)
      {
        contador=contador+1
        filas[contador]=i
      }
    }
  }
  
conta=0
  for( j in  1:n2) #columns
  {
    for( i in 1:n) #rows
    {
      if (salida[i,j]!=0)
      {
        conta=conta+1
        columnas[conta]=j 
      }
    }
  }
  
  minfil=min(filas)
  maxfil=max(filas)
  mincol=min(columnas)
  maxcol=max(columnas)
  
  dimension=c(minfil,maxfil,mincol,maxcol)
  dimension
}


# function to generate random networks  --------------------------------------------------

# Non-independent matrices

randomnetworks<-function(archivo,k,l,filmin,filmax,colmin,colmax,semilla)  # archivo= adjacency matrix. k= number of random matrices
{
  set.seed(semilla) #we have used different seed= {10,20,35,50,65,105,123,225,378,1001}
  salida1=read.table(here("data",archivo), header=TRUE, sep=" ") 
  matrizcut=salida1[filmin:filmax,colmin:colmax]
  dim1=dim(matrizcut)[1]
  out <- permatswap(matrizcut,method="swap",fixedmar = "both", times =k, burnin =l, thin =1, mtype = "prab")
  m=as.matrix(salida1)
  ig =graph_from_adjacency_matrix(m, mode="directed", weighted=TRUE)
  d=degree(ig, v = V(ig), mode = "total",loops = TRUE, normalized = FALSE)
  ig_ori =graph_from_adjacency_matrix(m, mode="undirected", weighted=TRUE)
  bet1=centr_betw(ig_ori, directed =FALSE)$res #betweenness of each node
  mt=as.matrix(salida1+t(salida1)) # adjacency matrix
  ig_orit =graph_from_adjacency_matrix(mt, mode="undirected", weighted=TRUE)
  wtc_orig_t <-cluster_louvain(ig_orit) 
  modula_t=modularity(ig_orit,membership(wtc_orig_t), directed =FALSE)
  c_ori_t=as.vector(modula_t)
  matriz1=as.matrix(d)
  n=dim(matriz1)
  matriznew=matrix(rep(0),n,2+k)
  matriznew[,1]=matriz1
  matriznew[,2]=bet1
  v=c()
  v[1]=c_ori_t
  
  salida2=salida1
  change=c()
  change[1]=0
  grupos=c()
  grupos[1]=length(wtc_orig_t)

  rownames(matriznew)=rownames(matriz1)
  for( j in  1:k)
  {
    salida1[filmin:filmax,colmin:colmax]=out$perm[[j]][,]
    change[j+1]=(dim(salida1)[1])^2-sum(salida2==salida1) 
   
    m=as.matrix(salida1+t(salida1))
    ig1 =graph_from_adjacency_matrix(m, mode="undirected", weighted=TRUE)
    
    bet=centr_betw(ig1, directed =FALSE)$res 
    wtc <- cluster_louvain(ig1) 
    grupos[j+1]=length(wtc)
    print(wtc)
    modula=modularity(ig1,membership(wtc), directed =FALSE)
    c=as.vector(modula)
    print(modula)
    matriznew[,2+j]=bet
    v[j+1]=c
    print(v)
    v2=cbind(v,change,grupos)
  }
  write.table(v2,file="data/modularity.csv")
}

# Network of pollinator-herbivore-plant species
generamosadyacencia("weighted_matrix.csv") #
region("matrizady.csv")
randomnetworks("matrizady.csv",40,0,1,36,37,47,1001)
