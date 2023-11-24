instalar_paquetes <- function(paquetes) {
  paquetes_faltantes <-
    paquetes[!paquetes %in% installed.packages()[, "Package"]]
  if (length(paquetes_faltantes)) {
    install.packages(paquetes_faltantes)
  }
}

# Lista de paquetes necesarios
paquetes_necesarios <- c("e1071", "pso")

# Llamar a la función
instalar_paquetes(paquetes_necesarios)
