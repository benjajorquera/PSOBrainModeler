library(signal)

# Generar escalón invertido y suavizado de presión
time <- seq(0, 59.5, 0.5) # tiempo en intervalos de 0.5 segundos
pressure_step <- c(rep(0, 60), rep(-0.5, 60)) # escalón invertido de presión
pressure_step_smooth <- butter(2, 0.2, type = "low", fs = 2) # filtro Butterworth de segundo orden

pressure_step_smooth <- filtfilt(pressure_step_smooth, pressure_step) # aplicar filtro al escalón de presión

pressure_step_smooth_norm <- (pressure_step_smooth - min(pressure_step_smooth)) / (max(pressure_step_smooth) - min(pressure_step_smooth)) # normalizar los datos

plot(time, pressure_step_smooth_norm, type = "l", xlab = "Tiempo", ylab = "Señal")

