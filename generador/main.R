setwd('/home/walter/generador')
nombres <- read.table(file = './datasets/nombres.txt', sep = "|", header = FALSE, fill = TRUE)
apellidos <- read.table(file = './datasets/apellidos.txt', sep = "|", header = FALSE, fill = TRUE)
correos <- read.table(file = './datasets/correos.txt', sep = "|", header = FALSE, fill = TRUE)
direcciones <- read.table(file = './datasets/direcciones.txt', sep = "|", header = FALSE, fill = TRUE)
direcciones_tipos <- read.table(file = './datasets/direcciones_tipos.txt', sep = "|", header = FALSE, fill = TRUE)
continentes <- read.table(file = './datasets/continentes.txt', sep = "|", header = FALSE, fill = TRUE)
idiomas <- read.table(file = './datasets/idiomas.txt', sep = "|", header = FALSE, fill = TRUE)
paises <- read.table(file = './datasets/paises.txt', sep = "|", header = FALSE, fill = TRUE)

nombre <- function (sexo = NULL, n = 1) {
    nombres_masculinos <- as.list(nombres[1,])
    nombres_femeninos <- as.list(nombres[2,])
    nombres <- c(nombres_masculinos, nombres_femeninos)
  
  if (n < 0) {
    stop('La cantidad debe ser mayor a 0')
  }
  
  if (is.null(sexo)) {
    # Cualquier genero
    return(sample(nombres, n))
  }
  
  if (sexo == 'M') {
    return(sample(nombres_masculinos, n))
  }
  
  if (sexo == 'F') {
    return(sample(nombres_masculinos, n))
  }
  
  stop('El sexo debe ser M, F o NULL')
}

apellido <- function (n = 1) {
  if (n >= 1 && n <= 2)
    return(sample(apellidos, n))
  
  stop('La cantidad de apellidos debe ser 1 o 2')
}

direccion <- function (n = 1) {
  if (n < 0) {
    stop('La cantidad debe ser mayor a 0')
  }
  
  dirs <- c()
  
  for (i in 1:n) {
    tipo <- sample(direcciones_tipos, 1)
    direccion <- sample(direcciones, 1)
    numero <- sample(100:999, 1)

    dirs <- append(dirs, paste(unlist(tipo), unlist(direccion), numero))
  }
  
  return(dirs)
}

telefono <- function (n = 1) {
  if (n < 0) {
    stop('La cantidad debe ser mayor a 0')
  }
  
  tels <- c()
  
  for (i in 1:n) {
    prim = sample(200:700, 1)
    seg = sample(0000:9999, 1)
    
    tels <- append(tels, paste(prim, '-', seg, sep = ''))
  }
  
  return(tels)
}

correo <- function (n = 1) {
  if (n < 0) {
    stop('La cantidad debe ser mayor a 0')
  }
  
  nombres_ <- c(as.list(nombres[1,]), as.list(nombres[2,]))
  
  emails <- c()
  seps <- c('-', '_', '.')
  
  for (i in 1:n) {
    prim = sapply(sample(nombres_, 1), tolower)
    seg = sapply(sample(apellidos, 1), tolower)
    separ = sample(seps, 1)
    dom = sample(correos, 1)
    
    emails <- append(emails, paste(unlist(prim), separ, unlist(seg), '@', unlist(dom), sep = ''))
  }
  
  return(emails)
}

fecha_hora <- function(formato = 'd-m-y', n = 1) {
  if (n < 0) {
    stop('La cantidad debe ser mayor a 0')
  }
  
  if (formato != 'd-m-y' && formato != 'y-m-d' && formato != 'd-m-y h:m:s' && formato != 'y-m-d h:m:s') {
    stop('Formato no valido')
  }
  
  fechas <- c()
  
    for (i in 1:n) {
      dia = sample(1:31, 1)
      mes = sample(1:12, 1)
      anio = sample(1000:2050, 1)
      
      if (formato == 'd-m-y') {
        fechas <- append(fechas, paste(dia, mes, anio, sep = '-'))
      }
      
      if (formato == 'y-m-d') {
        fechas <- append(fechas, paste(anio, mes, dia, sep = '-'))
      }
      
      if (formato == 'd-m-y h:m:s') {
        hora = sample(1:24, 1)
        min = sample(1:60, 1)
        seg = sample(1:60, 1)

        fechas <- append(fechas, paste(dia, '-', mes, '-', anio, ' ', hora, ':', min, ':', seg, sep = ''))
      }
      
      if (formato == 'y-m-d h:m:s') {
        hora = sample(1:24, 1)
        min = sample(1:60, 1)
        seg = sample(1:60, 1)
        
        fechas <- append(fechas, paste(anio, '-', mes, '-', dia, ' ', hora, ':', min, ':', seg, sep = ''))
      }
    }
    
    return(fechas)
}

ip <- function (version = 'v4',n = 1) {
  if (n < 0) {
    stop('La cantidad debe ser mayor a 0')
  }
  
  ips <- c()
  
  if (version == 'v4') {
    for (i in 1:n) {
      prim = sample(0:255, 1)
      seg = sample(0:255, 1)
      ter = sample(0:255, 1)
      cuar = sample(0:255, 1)
      
      ips <- append(ips, paste(prim, seg, ter, cuar, sep = '.'))
    }
    
    return(ips)
  }
  
  if (version == 'v6') {
    for (i in 1:n) {
      ips <- append(ips, paste(as.hexmode(sample(0:99999, 1)), as.hexmode(sample(0:99999, 1)), as.hexmode(sample(0:99999, 1)), as.hexmode(sample(0:99999, 1)), as.hexmode(sample(0:99999, 1)), as.hexmode(sample(0:99999, 1)), as.hexmode(sample(0:99999, 1)), as.hexmode(sample(0:99999, 1)), sep = ':'))
    }

    return(ips)
  }
  
  stop('La version debe ser v4 o v6')
}

continente <- function (n = 1) {
  if (n < 0) {
    stop('La cantidad debe ser mayor a 0')
  }
  
  conts <- c()
  
  for (i in 1:n) {
    cont = sample(continentes, 1)
    
    conts <- append(conts, cont)
  }
  
  return(conts)
}

pais <- function (continente = NULL, n = 1) {
  if (n < 0) {
    stop('La cantidad debe ser mayor a 0')
  }
  
  if (is.null(continente)) {
    paises_ = c()

    for(i in 1:NROW(paises)) {
      paises_ = append(paises_, paises[i,][-1])
    }
    
    return(sample(paises_, n))
  }
  
  if (continente == 'Africa') {
    paises_ = paises[1,]
    return(sample(paises_, n))
  }
  
  if (continente == 'America') {
    paises_ = paises[2,]
    return(sample(paises_, n))
  }
  
  if (continente == 'Asia') {
    paises_ = paises[3,]
    return(sample(paises_, n))
  }
  
  if (continente == 'Europa') {
    paises_ = paises[4,]
    return(sample(paises_, n))
  }
  
  if (continente == 'Oceania') {
    paises_ = paises[5,]
    return(sample(paises_, n))
  }
  
  stop('Continente no valido')
}

idioma <- function (formato = 'completo', n = 1) {
  if (n < 0) {
    stop('La cantidad debe ser mayor a 0')
  }
  
  if (formato == 'completo') {
    idiomas_ = idiomas[1,]
    
    return(sample(idiomas_, n))
  }
  
  if (formato == 'abreviado') {
    idiomas_ = idiomas[2,]
    return(sample(idiomas_, n))
  }
  
  stop('Formato no valido')
}

rango_numerico <- function (min = 0, max = 9999, tipo = 'entero', decimales = 2, n = 1) {
  if (n < 0) {
    stop('La cantidad debe ser mayor a 0')
  }
  
  if (tipo == 'entero') {
    return(sample(min:max, n))
  }
  
  if (tipo == 'real') {
    return(round(runif(n, min-0.01, max+0.01), decimales))
  }
  
  stop('Tipo de numero no valido')
}

codigo_upc <- function (n = 1) {
  if (n < 0) {
    stop('La cantidad debe ser mayor a 0')
  }
  
  codigos <- c()
  
  for (i in 1:n) {
    prim = sample(2000:2020, 1)
    seg = sample(10000:99999, 1)
    
    codigos <- append(codigos, paste('u', prim, seg, sep = ''))
  }
  
  return(codigos) 
}
