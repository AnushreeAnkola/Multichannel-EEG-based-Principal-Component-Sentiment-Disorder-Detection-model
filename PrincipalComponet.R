setwd("C:/Users/Aishwarya/Desktop")

# THE MAIN FUNCTION
my.pca <- function()
{
  # READ DATA FROM CSV FILE
  my.data <- my.read("short_data.csv")
  
  # STORE THE NAME OF SUBJ AND SENTIMENTAL ELECTRODES
  processed.data <- my.data[c(1,3,4,5,6,7,8,9,10,11,12,15,16)]
  processed.data$C3 <- as.numeric(processed.data$C3)
  
  # CHOOSE 1 SUBJ FOR PCA; PROBLEM : TAKE VALUE FROM USER
  print( " Choose one SUBJ in the Program.
         1: CBA
         2: CLM
         3: EGA
         4: FSA
         5: GMI
         6: GRO
         7: HTH
         8: MBA
         9: MMA
         10: MTA
         11: PLA
         12: SCE 
         13: SPH
         14: WPA
         ")
  
  # SWITCH STATEMENT TO SELECT ONE OF THE SUBJ
  ANSWER1 = readline(prompt = "enter the name of the subject from the above list. (Should be in capital) :- ")
  
  switch(ANSWER1,
         CBA = {
           CBA <- subset(processed.data,SUBJECT == "CBA")
           yes <- CBA[-c(1)]
         },
         CLM = {
           CLM <- subset(processed.data,SUBJECT == "CLM")
           yes <- CLM[-c(1)]
         },
         EGA = {
           EGA <- subset(processed.data,SUBJECT == "EGA")
           yes <- EGA[-c(1)]
         },
         FSA = {
           FSA <- subset(processed.data,SUBJECT == "FSA")
           yes <- FSA[-c(1)]
         },
         GMI = {
           GMI <- subset(processed.data,SUBJECT == "GMI")
           yes <- GMI[-c(1)]
         },
         GRO = {
           GRO <- subset(processed.data,SUBJECT == "GRO")
           yes <- GRO[-c(1)] 
         },
         HTH = {
           HTH <- subset(processed.data,SUBJECT == "HTH")
           yes <- HTH[-c(1)]
         },
         MBA = {
           MBA <- subset(processed.data,SUBJECT == "MBA")
           yes <- MBA[-c(1)]
         },
         MMA = {
           MMA <- subset(processed.data,SUBJECT == "MMA")
           yes <- MMA[-c(1)]
         },
         MTA = {
           MTA <- subset(processed.data,SUBJECT == "MTA")
           yes <- MTA[-c(1)]
         },
         PLA = {
           PLA <- subset(processed.data,SUBJECT == "PLA")
           yes <- PLA[-c(1)]
         },
         SCE = {
           SCE <- subset(processed.data,SUBJECT == "SCE")
           yes <- SCE[-c(1)]
         },
         SPH = {
           SPH <- subset(processed.data,SUBJECT == "SPH")
           yes <- SPH[-c(1)]
         },
         WPA = {
           WPA <- subset(processed.data,SUBJECT == "WPA")
           yes <- WPA[-c(1)]
         }
  )
  
  # SAVE ANS IN ANOTHER VARIABLE
  prom <- yes
  
  # CHOOSE EITHER SUBSET OF DATA OR SERIES OF DATA
  
  ANSWER <- readline("1 : series 2: random  ----> ")
  
  if (substr(ANSWER, 1, 1) == "1")
    rand.set <- series(prom,100)
  else
    rand.set <- PCADecision.Boundary(prom)
  View(rand.set)
  
  # plot(rand.set,col="blue",main="Graph1")
  
  # PERFORM PCA
  my.pr <- my.princomp.function(rand.set)
  print(my.pr)
  
  #FIND EIGEN VALUES
  std.dev <- choosing.pc(my.pr)
  print("The eigen values according to Kaiser's criterion are:")
  print(std.dev)
  
  #USED PCA - HIGH VARIANCE
  now <- comp.used(std.dev) 
  print(now)
  
  # MAX PCA TOBE SELECTED
  x <- print(max(now))
  
  # THE OUTPUT OF PCA TO BE FEEDED TO SENTIMENTAL ANALYSIS
  loadings <- as.data.frame(my.pr$loadings[,1:x])
  write.csv(loadings,"output.res.csv")
  
  View(loadings)
  
  
  X<-read.csv("output.res.csv") 
  
  #extracting data subject wise(row-wise)
  view_sort_data<-sort_data(X)
  
  
  ANSWER2 = readline(prompt = "press A for corelation  :- ")
  
  switch(ANSWER2,
         A = {
           A<-plot(loadings,col="magenta")
         })
         ANSWER3 = readline(prompt = " press B for biplot :- ")
         
   switch(ANSWER3,
               
         B= {
           B<-biplot(my.pr, cex=c(1, 0.7))
         })
  
  #scatter
  
  print("This is for princomp()")
  
  
}

sort_data<-function(X)
{
  obj<-X
  obj
  obj1<-obj[-c(1)]
  obj1<-colSums(obj1)
  obj1
  barplot(obj1)
  
}  

series <- function(x,n)
{
  head(x,n)
}

comp.used <- function(x)
{
  see <- which(x[]>1)
  print("The component to be used are :")
  return(see)
}

choosing.pc <- function(x)
{
  #Kaiser criterion
  me <- x$sdev^2
  return(me)
}
my.read <- function(file, header = TRUE, sep = ",",...)
{
  read.csv(file, header = TRUE)
}

PCADecision.Boundary <- function(x)
{
  see <- x[sample(nrow(x), 100), ]
  return(see)
}

my.princomp.function <- function(x, cor = TRUE, scores = TRUE,...)
{
  now <- princomp(x, cor = TRUE, scores = TRUE)
  return(now)
}

#------------HPC----------------
Time_sys <- function()
{
  system.time(my.pca())
  
}
# run this on console----- Time_sys()

profile_Rprof <- function()
{
  Rprof(filename = "profile.out", memory.profiling = FALSE, line.profiling = TRUE)
  my.pca()
  Rprof(NULL)
  summaryRprof("profile.out",lines = "show")
}
# run this on console ---profile_Rprof()
