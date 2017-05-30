
#EJEMPLO PARA CREAR TABLE:  design.mat(c(2,3,3))

design.mat <-function(f){
source("D:/Funciones del R/fullfact.R")

	m<-fullfact(f)

	N<-dim(m)[1]
	l<-dim(m)[2]

	fname<-toString(f)
	sub(", ","x",fname)->fname
	sub(", ","x",fname)->fname
	sub(", ","x",fname)->fname
	sub(", ","x",fname)->fname
	paste(fname,".h",sep="")->fname			
	zz<-file(fname,"w")
	cat("int table[",file=zz)
	cat(N,file=zz)
	cat("][",file=zz)
	cat(l,file=zz)
	cat("]={","\n",file=zz)
	for(i in 1:N){
		if(l==2){
		
		li<-sprintf("{%.0f, %.0f},",m[i,1],m[i,2])
		}else if(l==3){
		li<-sprintf("{%.0f, %.0f, %.0f},",m[i,1],m[i,2],m[i,3])
		
		}else if(l==4){
		li<-sprintf("{%.0f, %.0f, %.0f, %.0f},",m[i,1],m[i,2],m[i,3],m[i,4])
		}else{
		li<-sprintf("{%.0f, %.0f, %.0f, %.0f, %.0f},",m[i,1],m[i,2],m[i,3],m[i,4],m[i,5])
		
		}
		
		cat(li,file=zz,sep="\n")
		
	}
	cat("};",file=zz,sep="\n")
	close(zz)
	#rm(fullfact)
}

