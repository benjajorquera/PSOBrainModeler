# File:				ARSVM_sim.r
# Purpose:			Fit AR (Autoregressive) and ARSVM (Autoregressive Support Vector Machine) 
#					models to simulated data		
# Input:			N/A
# Output:			Plots of 1)simulated data, 2)model fits, 3)model residuals
# Author:			Luke Bornn (http://www.stat.ubc.ca/~l.bornn)
#					- Thanks for the acknowledgment! -
# Requires:			Libraries 'e1071' & 'tseries'
# Reference:		Bornn, L., Farrar, C.R., Park, G., Farinholt,K., (2009) 
#					“Structural Health Monitoring With Autoregressive Support Vector Machines” 
#					Journal of Vibration And Acoustics, 131, 021004.
# Last Modified:	May 6, 2009

# Housekeeping
	rm(list=ls()); set.seed(2);
	library(e1071); library(tseries);
	
	
# Create Simulated Data
	T = 1200;
	
	# Original Signal
		simdata = sin(400*pi*(1:T)/1200)^3 + sin(400*pi*(1:T)/1200)^2 + sin(200*pi*(1:T)/1200) +  sin(100*pi*(1:T)/1200) + rnorm(T,0,.1);

	# Add Anomalies
		simdata[600:620] = simdata[600:620] + rnorm(21,0,.5);
		simdata[800:850] = simdata[800:850] + .5*sin(1000*pi*(820:870)/1200);
		simdata[1000:1020] = simdata[1000:1020] + rnorm(21,1,.2);


# Fit ARSVM Model
	p = 5; 				# Order of AR model
	trainlength = 400;	# Length of training data
	
	# Format data into AR (matrix) format for fitting (and testing)
		x_train = matrix(simdata[rep(1:(trainlength-p), each = p) + rep(0:(p-1), (trainlength-p))], 
					nrow = (trainlength-p), ncol = p, byrow = TRUE);
		x_test = matrix(simdata[rep(1:(T-p), each = p) + rep(0:(p-1), (T-p))], 
					nrow = T-p, ncol = p, byrow = TRUE);
		y_train = simdata[(p+1):trainlength];
		y_test = simdata[(p+1):T];
    		
	# Fit ARSVM model and create predictions
		modelsvm = svm(x_train,y_train);
		modelsvm_pred = predict(modelsvm,x_test);
		
	# Fit Traditional AR model and create predictions
		modelar = ar.yw(x=simdata[1:400],order=p);
		modelar_pred = rep(0,(T-p));
		for(i in 1:(T-p)){
	 		modelar_pred[i] = predict(modelar, newdata=x_test[i,])$pred[1];
		}

	
# Create Plots
	par(mfrow = c(1,1));
	plot(y_test,type="l",ylim=c(-2.5,3.5), xlab="Time", ylab="Amplitude", main="Simulated Data");
	
	# Plot subsection of data with both models fitted
		par(mfrow = c(2,1));
		plot(y_test[(trainlength+1):600],type="l",x=((trainlength+1):600),ylim=c(-2.5,3.5), xlab="Time", ylab="Amplitude", main = "Simulated Data with ARSVM Fit");
		points(modelsvm_pred[(trainlength+1):(600)],type="l",lty="dashed",x=((trainlength+1):600));
		
		plot(y_test[(trainlength+1):600],type="l",x=((trainlength+1):600),ylim=c(-2.5,3.5), xlab="Time", ylab="Amplitude", main = "Simulated Data with AR Fit");
		points(modelar_pred[(trainlength+1):(600)],type="l",lty="dashed",x=((trainlength+1):600));
	

	# Plot residuals from each model's fit with confidence bands
		par(mfrow = c(2,1));
		plot(y_test[(trainlength+1):(T-p)] - modelsvm_pred[(trainlength+1):(T-p)], type="l", x=((trainlength+1):(T-p)),ylim=c(-3,3), xlab="Time", ylab="Amplitude", main = "Residuals from ARSVM fit");
		cl <- quantile((y_train-modelsvm_pred[1:(400-p)]),probs=c(.01,.99));
		points(rep(cl[1],800), lty="dotted", type="l", x=((trainlength+1):T));
		points(rep(cl[2],800), lty="dotted", type="l", x=((trainlength+1):T));
		
    	
		plot(y_test[(trainlength+1):(T-p)] - modelar_pred[(trainlength+1):(T-p)], type="l", x=((trainlength+1):(T-p)),ylim=c(-3,3), xlab="Time", ylab="Amplitude", main = "Residuals from AR fit");
		cl <- quantile((y_train-modelar_pred[1:(400-p)]),probs=c(.01,.99));
		points(rep(cl[1],800-p), lty="dotted", type="l", x=((trainlength+1):(T-p)));
		points(rep(cl[2],800-p), lty="dotted", type="l", x=((trainlength+1):(T-p)));
	
	

# Copyright 2009 Luke Bornn.  Please email suggestions, comments and corrections to l.bornn(at)stat.ubc.ca
