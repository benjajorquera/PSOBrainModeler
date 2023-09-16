
setwd("C:/Users/rodrigo/Desktop/Datos/ARX_Base12/TODOS/Base");
#setwd("C:/Users/rodrigo/Desktop/Datos/pruebas");

files <-list.files();
count <- 0;
localMaxima <- function(x) {
  y <- diff(c(-.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}


for (f in files) {
  nom <- strsplit(f,".txt");
  mydata = scan( f, what=0);
  range <- max(mydata) - min(mydata);
  mydata <- (mydata-min(mydata))/range;
  range2 <- 1.2 + 0.2;
  mydata <- (mydata*range2) - 0.2;
  ret_PAM_min <- 2;
  ret_PAM_max <- 10;
  maximos_entre_minimos_aux <- numeric(0);
  for(ret in ret_PAM_min:ret_PAM_max){
    porcentaje <- 0;
    puntaje <- 0;
    corte <- ret;
    delta <- 0;
    ajuste1 <- mydata[(100 - corte)] - 0.8;
    caida <- min(mydata[(100 - corte):(110 - corte)]-ajuste1); 
    varEstabilizacion <- var(mydata[(128 - corte):(145 - corte)]-ajuste1); 
    min <- min(mydata[(95 - corte):(145 - corte)]-ajuste1);
    max <- max(mydata[(95 - corte):(145 - corte)]-ajuste1);
    
    if( caida <= 0.3 && caida >= -0.2 && varEstabilizacion < 0.002 && min > -0.2 && max < 1.2){
      min_loc <- localMaxima(-(mydata[(100-corte):(128-corte)]-ajuste1));
      if(length(min_loc) > 1){
        castigo <- 1;
        for(i in 1:(length(min_loc)-1)){
          maximo_entre_minimos <- localMaxima(mydata[(100+min_loc[i]-1-corte):(100+min_loc[(i+1)]-1-corte)]-ajuste1);
          maximos_entre_minimos_aux <- c(maximos_entre_minimos_aux,maximo_entre_minimos);
          if(length(maximo_entre_minimos) != 0){
            distancia_entre_min_max <- (mydata[(100+min_loc[i]+maximo_entre_minimos-corte-1)] -ajuste1) - (mydata[(100+min_loc[1]-corte-1)] - ajuste1);
            distancia_maxima <- abs(distancia_entre_min_max*0.45);
            distancia_entre_max_min <- abs((mydata[(100+min_loc[1]+maximo_entre_minimos-corte-1)] -ajuste1) - (mydata[(100+min_loc[i+1]-corte-1)] - ajuste1));
            if((distancia_maxima >= distancia_entre_max_min) && (distancia_entre_max_min >= 0.009)){
              if(castigo > 1){
                porcentaje <- (distancia_entre_max_min/distancia_entre_min_max + porcentaje)/castigo*distancia_maxima;
              }else{
                porcentaje <- distancia_entre_max_min/distancia_entre_min_max + porcentaje;
              }
              castigo <- castigo + 1;
            }else if(distancia_entre_max_min < 0.009){
              delta <- abs(mydata[(100+maximo_entre_minimos-corte)]-ajuste1 - mydata[145-corte]-ajuste1) + porcentaje;
            }else{
              porcentaje <- (distancia_entre_max_min*distancia_entre_min_max + porcentaje)*0.1;
            }
          }
        }
        if( (round(mydata[(100-corte+min_loc[1]-1)]-ajuste1,digits=1) != 0.8) && (round(mydata[(95-corte)]-ajuste1,digits=1) == 0.8) && (round(mydata[(95-corte + 7)]-ajuste1,digits=1) != 0.8) && (round(mydata[(95-corte + 6)]-ajuste1,digits=1) == 0.8) ){
          if(delta == 0){
            puntaje <- round(porcentaje/(varEstabilizacion*1000),digits=1)
          }else{
            puntaje <- round(delta/(varEstabilizacion*1000),digits=1)
          }
          if(puntaje > 10){
            puntaje <- 10;
            mypath <- file.path("C:","Users","rodrigo","Desktop","PROGRAMA_R","AB",paste(nom," [RET = ",ret, "] [PUNTAJE =  ",puntaje,"].png", sep = ""));
            #mypath <- file.path("C:","Users","rodrigo","Desktop","PROGRAMA_R","pruebas",paste(nom," [RET = ",ret, "] [PUNTAJE =  ",puntaje,"].png", sep = ""));
            png(file=mypath);
            plot(mydata[(95 - corte):(145 - corte)] - ajuste1,type='o',pch=8,col='red',xlab="Tiempo (s)", ylab="VFSC Estimado (cm/seg)",ylim=c(-0.2,1.2),xlim=c(0,50));
            #points(min_loc+5,mydata[(100+min_loc-corte-1)]-ajuste1,pch=19,col=27);
            #points(min_loc[1] + 5 +maximos_entre_minimos_aux,mydata[(100+min_loc[1]+maximos_entre_minimos_aux-corte-1)]-ajuste1,pch=2);
            count <- count +1;
            dev.off(); 
          }else if(puntaje < 1){
            puntaje <- 1;
            mypath <- file.path("C:","Users","rodrigo","Desktop","PROGRAMA_R","AB",paste(nom," [RET = ",ret, "] [PUNTAJE =  ",puntaje,"].png", sep = ""));
            #mypath <- file.path("C:","Users","rodrigo","Desktop","PROGRAMA_R","pruebas",paste(nom," [RET = ",ret, "] [PUNTAJE =  ",puntaje,"].png", sep = ""));
            png(file=mypath);
            plot(mydata[(95 - corte):(145 - corte)] - ajuste1,type='o',pch=8,col='red',xlab="Tiempo (s)", ylab="VFSC Estimado (cm/seg)",ylim=c(-0.2,1.2),xlim=c(0,50));
            #points(min_loc+5,mydata[(100+min_loc-corte-1)]-ajuste1,pch=19,col=27);
            #points(min_loc[1] + 5 +maximos_entre_minimos_aux,mydata[(100+min_loc[1]+maximos_entre_minimos_aux-corte-1)]-ajuste1,pch=2);
            count <- count +1;
            dev.off();
          }else{
            mypath <- file.path("C:","Users","rodrigo","Desktop","PROGRAMA_R","AB",paste(nom," [RET = ",ret, "] [PUNTAJE =  ",puntaje,"].png", sep = ""));
            #mypath <- file.path("C:","Users","rodrigo","Desktop","PROGRAMA_R","pruebas",paste(nom," [RET = ",ret, "] [PUNTAJE =  ",puntaje,"].png", sep = ""));
            png(file=mypath);
            plot(mydata[(95 - corte):(145 - corte)] - ajuste1,type='o',pch=8,col='red',xlab="Tiempo (s)", ylab="VFSC Estimado (cm/seg)",ylim=c(-0.2,1.2),xlim=c(0,50));
            #points(min_loc+5,mydata[(100+min_loc-corte-1)]-ajuste1,pch=19,col=27);
            #points(min_loc[1] + 5 +maximos_entre_minimos_aux,mydata[(100+min_loc[1]+maximos_entre_minimos_aux-corte-1)]-ajuste1,pch=2);
            count <- count +1;
            dev.off();
          }
          
        }
      }else{
        if( (round(mydata[(100-corte+min_loc[1]-1)]-ajuste1,digits=1) != 0.8) && (round(mydata[(95-corte)]-ajuste1,digits=1) == 0.8) && (round(mydata[(95-corte + 7)]-ajuste1,digits=1) != 0.8) && (round(mydata[(95-corte + 6)]-ajuste1,digits=1) == 0.8) ){
          maximos <- localMaxima(mydata[(100+min_loc[1]-corte-1):(145-corte-1)]-ajuste1);
          aux <- min_loc +1;
          for(j in aux:50){
            if(round(mydata[(100-corte+j+1)]-ajuste1 - (mydata[(100-corte+j)]-ajuste1),digits=3) <= 0.009){
              copia <- mydata[(100-corte+j+1)]-ajuste1;
              break
            }
          }
          delta <- abs(copia - (mydata[(100-corte+maximos-1+min_loc[1])]-ajuste1));#diferencia entre estabilización y pto máx
          puntaje <- round(delta/(varEstabilizacion*1000),digits=1);#se calcula el puntaje asignado
          if(puntaje > 10){
            puntaje <- 10;
            mypath <- file.path("C:","Users","rodrigo","Desktop","PROGRAMA_R","AB",paste(nom," [RET = ",ret, "] [PUNTAJE =  ",puntaje,"].png", sep = ""));
            #mypath <- file.path("C:","Users","rodrigo","Desktop","PROGRAMA_R","pruebas",paste(nom," [RET = ",ret, "] [PUNTAJE =  ",puntaje,"]RARO.png", sep = ""));
            png(file=mypath);
            plot(mydata[(95 - corte):(145 - corte)] - ajuste1,type='o',pch=8,col='red',xlab="Tiempo (s)", ylab="VFSC Estimado (cm/seg)",ylim=c(-0.2,1.2),xlim=c(0,50));
            #points(min_loc+5,mydata[(100+min_loc-corte-1)]-ajuste1,pch=19,col=27);
            #points(min_loc[1]+maximos+5,mydata[(100+min_loc[1]+maximos-corte-1)]-ajuste1,pch=2);
            count <- count +1;
            dev.off();
          }else if(puntaje < 1){
            puntaje <- 1;
            mypath <- file.path("C:","Users","rodrigo","Desktop","PROGRAMA_R","AB",paste(nom," [RET = ",ret, "] [PUNTAJE =  ",puntaje,"].png", sep = ""));
            #mypath <- file.path("C:","Users","rodrigo","Desktop","PROGRAMA_R","pruebas",paste(nom," [RET = ",ret, "] [PUNTAJE =  ",puntaje,"]RARO.png", sep = ""));
            png(file=mypath);
            plot(mydata[(95 - corte):(145 - corte)] - ajuste1,type='o',pch=8,col='red',xlab="Tiempo (s)", ylab="VFSC Estimado (cm/seg)",ylim=c(-0.2,1.2),xlim=c(0,50));
            #points(min_loc+5,mydata[(100+min_loc-corte-1)]-ajuste1,pch=19,col=27);
            #points(min_loc[1]+maximos+5,mydata[(100+min_loc[1]+maximos-corte-1)]-ajuste1,pch=2);
            count <- count +1;
            dev.off();
          }else{
            mypath <- file.path("C:","Users","rodrigo","Desktop","PROGRAMA_R","AB",paste(nom," [RET = ",ret, "] [PUNTAJE =  ",puntaje,"].png", sep = ""));
            #mypath <- file.path("C:","Users","rodrigo","Desktop","PROGRAMA_R","pruebas",paste(nom," [RET = ",ret, "] [PUNTAJE =  ",puntaje,"]RARO.png", sep = ""));
            png(file=mypath);
            plot(mydata[(95 - corte):(145 - corte)] - ajuste1,type='o',pch=8,col='red',xlab="Tiempo (s)", ylab="VFSC Estimado (cm/seg)",ylim=c(-0.2,1.2),xlim=c(0,50));
            #points(min_loc+5,mydata[(100+min_loc-corte-1)]-ajuste1,pch=19,col=27);
            #points(min_loc[1]+maximos+5,mydata[(100+min_loc[1]+maximos-corte-1)]-ajuste1,pch=2);
            count <- count +1;
            dev.off();
          }
        }
      }
    }else{
      if( (round(mydata[(100-corte+min_loc[1]-1)]-ajuste1,digits=1) != 0.8) && (round(mydata[(95-corte)]-ajuste1,digits=1) == 0.8) && (round(mydata[(95-corte + 7)]-ajuste1,digits=1) != 0.8) && (round(mydata[(95-corte + 6)]-ajuste1,digits=1) == 0.8) ){
        count <- count +1;
      }
    }
  }
}
