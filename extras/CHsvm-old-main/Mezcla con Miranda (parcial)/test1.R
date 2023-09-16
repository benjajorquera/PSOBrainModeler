# Based on "ARSVM_sim.R" by Luke Bornn (http://www.stat.ubc.ca/~l.bornn)

rm(list=ls());
require(e1071);
require(tseries);
source("/research/SVM/Test1/Signal-simulation.R")

n.cardiac.cycles = 1000
ari.values = c(3.5, 5, 8)
seed = 2
sampling.time <- 0.6

# Create Simulated Data
raw.abp <- getSyntheticABPSignal(n.cardiac.cycles = n.cardiac.cycles, seed = seed)
raw.signals <- getSyntheticABPAndCBFVSignals(raw.abp, at.ari = ari.values)
mean.signals <- getMeanSyntheticSignals(raw.signals)

t.ini <- mean.signals[1, "Time"]
t.end <- mean.signals[nrow(mean.signals), "Time"]
t <- seq(t.ini, t.end, sampling.time)
sampled.signals <- resampleMeanSyntheticSignal(mean.signals, t)
norm.signals <- normaliseMeanSyntheticSignal(sampled.signals)

abp <- norm.signals[["Mean_ABP"]]
cbfv <- norm.signals[["Mean_CBFV.ARI=3.5"]]
n.samples <- length(abp)

# Fit ARSVM Model
ar.model.order <- 5; 				# Order of AR model
train.length = as.integer(n.samples / 2);	# Length of training data

# Format data into AR (matrix) format for fitting (and testing)
x.train = matrix(abp[rep(1:(train.length - ar.model.order), each = ar.model.order) +
          rep(0:(ar.model.order - 1), (train.length - ar.model.order))], 
          nrow = (train.length - ar.model.order), ncol = ar.model.order, byrow = TRUE);
x.test = matrix(abp[rep(1:(n.samples - ar.model.order), each = ar.model.order) +
         rep(0:(ar.model.order - 1), (n.samples - ar.model.order))], 
         nrow = n.samples - ar.model.order, ncol = ar.model.order, byrow = TRUE);
y.train = cbfv[(ar.model.order + 1):train.length];
y.test = cbfv[(ar.model.order + 1):n.samples];

# Fit ARSVM model
svm.ar.model = svm(x.train, y.train);

# Get validation predictions
valid.pred = predict(svm.ar.model, x.test);

# Read theoretical step
tstep.dir <- file.path("", "research", "SVM", "Test1")
tstep.file.sampling.time <- paste0(0, as.character(sampling.time * 10), "s")
tstep.file.basename <- paste("ABP", "step", tstep.file.sampling.time, sep = "-")
tstep.file.basename <- paste(tstep.file.basename, "csv", sep = ".")
tstep.filename <- file.path(tstep.dir, tstep.file.basename)
tstep <- read.csv(tstep.filename, sep = ";")
tstep.time <- tstep[["Time"]]
tstep.abp <- tstep[["ABP"]]

# Create predictions for theoretical step
n.tstep.samples <- length(tstep.abp)
x.tstep = matrix(tstep.abp[rep(1:(n.tstep.samples - ar.model.order), each = ar.model.order) +
         rep(0:(ar.model.order - 1), (n.tstep.samples - ar.model.order))], 
         nrow = n.tstep.samples - ar.model.order, ncol = ar.model.order, byrow = TRUE);
tstep.pred = predict(svm.ar.model, x.tstep);

# Get step segment (second -10 up to 20)
i <- as.integer(351/ sampling.time);
j <- as.integer(380/ sampling.time) + 1;
step.samples <- i:j

time.step <- tstep.time[step.samples]
abp.step <- tstep.abp[step.samples]
cbfv.step <- tstep.pred[step.samples - ar.model.order + 1]

# Prepare data for plotting
d.wide <- data.frame(Time = time.step, Value.ABP = abp.step, Value.CBFV = cbfv.step)
d.long <- reshape(d.wide, varying = 2:3, idvar = "Time", timevar = "Signal",
                  direction = "long")
d.long[["Signal"]] <- factor(d.long[["Signal"]])
min.y <- min(d.long[["Value"]]) - 1
max.y <- max(d.long[["Value"]]) + 1

# Create Plots
plot1 <- ggplot(d.long, aes(x = Time, y = Value, colour = Signal)) +
         geom_line() +
          xlab("Time") + ylab("Normalised Signal Amplitude")
print(plot1)

stop()
	
# Create Plots
par(mfrow = c(1,1));
plot(y.test,type="l",ylim=c(-2.5,3.5), xlab="Time", ylab="Amplitude", main="Simulated Data");
	
	# Plot subsection of data with both models fitted
		par(mfrow = c(2,1));
		plot(y.test[(train.length+1):600],type="l",x=((train.length+1):600),ylim=c(-2.5,3.5), xlab="Time", ylab="Amplitude", main = "Simulated Data with ARSVM Fit");
		points(svm.ar.model.pred[(train.length+1):(600)],type="l",lty="dashed",x=((train.length+1):600));
		
		plot(y.test[(train.length+1):600],type="l",x=((train.length+1):600),ylim=c(-2.5,3.5), xlab="Time", ylab="Amplitude", main = "Simulated Data with AR Fit");
		points(ar.model.pred[(train.length+1):(600)],type="l",lty="dashed",x=((train.length+1):600));
	

	# Plot residuals from each model's fit with confidence bands
		par(mfrow = c(2,1));
		plot(y.test[(train.length+1):(T-p)] - svm.ar.model.pred[(train.length+1):(T-p)], type="l", x=((train.length+1):(T-p)),ylim=c(-3,3), xlab="Time", ylab="Amplitude", main = "Residuals from ARSVM fit");
		cl <- quantile((y.train-svm.ar.model.pred[1:(400-p)]),probs=c(.01,.99));
		points(rep(cl[1],800), lty="dotted", type="l", x=((train.length+1):T));
		points(rep(cl[2],800), lty="dotted", type="l", x=((train.length+1):T));
		
    	
		plot(y.test[(train.length+1):(T-p)] - ar.model.pred[(train.length+1):(T-p)], type="l", x=((train.length+1):(T-p)),ylim=c(-3,3), xlab="Time", ylab="Amplitude", main = "Residuals from AR fit");
		cl <- quantile((y.train-ar.model.pred[1:(400-p)]),probs=c(.01,.99));
		points(rep(cl[1],800-p), lty="dotted", type="l", x=((train.length+1):(T-p)));
		points(rep(cl[2],800-p), lty="dotted", type="l", x=((train.length+1):(T-p)));
	
	

# Original Signal
simdata <- sin(400*pi*(1:T)/1200)^3 + sin(400*pi*(1:T)/1200)^2 + sin(200*pi*(1:T)/1200) +  sin(100*pi*(1:T)/1200);# + rnorm(T,0,.1);

# Prepare data for plotting
d <- data.frame(Time = 1:T, Signal = simdata)

plot1 <- ggplot(d, aes(x = Time, y = Signal)) +
        geom_line() +
        xlab("Time") + ylab("Signal")
print(plot1)

stop()

# Add Anomalies
nlim <- 5
lim.inf <- c(1, (as.integer(T / nlim) * 1:(nlim - 1)) + 1)
lim.sup <- c(as.integer(T / nlim) * 1:(nlim - 1), T)
#~ print(lim.inf)
#~ print(lim.sup)

simdata[lim.inf[2]:lim.sup[2]] = simdata[lim.inf[2]:lim.sup[2]] + rnorm(lim.sup[2]-lim.inf[2]+1,0,.5);
simdata[lim.inf[3]:lim.sup[3]] = simdata[lim.inf[3]:lim.sup[3]] + .5*sin(1000*pi*(lim.inf[3]:lim.sup[3]) / T);
simdata[lim.inf[4]:lim.sup[4]] = simdata[lim.inf[4]:lim.sup[4]] + rnorm(lim.sup[4]-lim.inf[4]+1,1,.2);
#~ print(simdata)
