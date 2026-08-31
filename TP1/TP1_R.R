# =============================================================================
# TP 1 -- La naturaleza de la econometria y los datos economicos (R)
# Alumna: Lucia Vazquez | Materia: Econometria I | Fecha: 31/08/2026
# Referencia: Wooldridge, Introduccion a la econometria, capitulo 1
#
# Resuelve en R los ejercicios 1.2, 1.5, 1.6, 1.7 y 1.8.
# El desarrollo teorico completo (pregunta economica, especificacion,
# inferencia, interpretacion y limitaciones, segun la plantilla de entrega)
# esta en TP1_entrega.docx; este script documenta el codigo y una sintesis
# breve de cada respuesta. La version equivalente en Python esta en
# TP1_python.ipynb.
# =============================================================================

library(wooldridge)

# =============================================================================
# Ejercicio 1.2 (Wooldridge C1.2, bwght)
# Cuantas mujeres hay en la muestra, cuantas fumaron durante el embarazo,
# el promedio de cigs y ese promedio solo entre fumadoras.
# =============================================================================
data(bwght, package = "wooldridge")

nrow(bwght)                          # tamano de la muestra (mujeres)
sum(bwght$cigs > 0)                  # cuantas fumaron
mean(bwght$cigs)                     # promedio sobre toda la muestra
mean(bwght$cigs[bwght$cigs > 0])     # promedio solo entre fumadoras

# Salida esperada:
#   n = 1388
#   fumadoras = 212
#   media (todas) = 2.087
#   media (fumadoras) = 13.665
#
# Sintesis: 1,388 mujeres, 212 (15.3%) fumaron durante el embarazo. El
# promedio general (2.09) diluye el 84.7% de ceros; el promedio condicional
# (13.67) si describe la intensidad de consumo de quien fuma, y es el que
# responde la pregunta planteada. Ambos numeros probablemente subestiman el
# consumo real por subdeclaracion de una conducta estigmatizada.


# =============================================================================
# Ejercicio 1.5 (wage1)
# Salario promedio por hora de varones y mujeres, y tres variables del
# dataset que podrian explicar parte de la brecha sin invocar discriminacion.
# =============================================================================
data(wage1, package = "wooldridge")

aggregate(wage ~ female, data = wage1, FUN = mean)

brecha_abs <- mean(wage1$wage[wage1$female == 0]) - mean(wage1$wage[wage1$female == 1])
brecha_pct <- 100 * brecha_abs / mean(wage1$wage[wage1$female == 0])
c(brecha_abs = brecha_abs, brecha_pct = brecha_pct)

aggregate(cbind(educ, exper, tenure) ~ female, data = wage1, FUN = mean)
aggregate(cbind(profocc, clerocc, servocc) ~ female, data = wage1, FUN = mean)

# Especificacion equivalente como regresion simple (dummy = comparacion de medias)
m <- lm(wage ~ female, data = wage1)
summary(m)

# Prueba de hipotesis H0: misma media (equivalente al t del modelo anterior)
t.test(wage ~ female, data = wage1, var.equal = TRUE)

# Con tenure como control, para cuantificar el sesgo por omitirla
m2 <- lm(wage ~ female + tenure, data = wage1)
summary(m2)

# Salida esperada (redondeada):
#   media varones = 7.0995, media mujeres = 4.5877
#   brecha = 2.512 USD/hora (35.4% del salario de los varones)
#   wage_hat = 7.0995 - 2.5118*female,  SE (0.2100) (0.3034), n=526, R2=0.116
#   t = -8.28, p ~ 1e-15
#   con tenure: coef. de female pasa de -2.5118 a -2.0865
#
# Sintesis: brecha de 2.51 USD/hora (35.4%), estadisticamente distinguible
# de cero (p ~ 1e-15). Tres variables que explican parte de ella sin invocar
# discriminacion: tenure (las mujeres promedian menos antiguedad: 3.62 vs.
# 6.47 anios), la distribucion ocupacional (mas concentracion femenina en
# clerical/servicios y menos en profesional) y exper (16.43 vs. 17.56 anios).
# Controlando por tenure, el coeficiente de female baja de -2.51 a -2.09:
# parte de la brecha bruta refleja la menor antiguedad, no necesariamente
# un trato distinto en el mismo puesto.


# =============================================================================
# Ejercicio 1.6 (aproximacion logaritmica)
# Cambio porcentual exacto vs. aproximacion logaritmica: 6,000 vs. 5,500
# dolares de gasto por alumno, y 5,500 vs. 11,000.
# =============================================================================
cambio <- function(x0, x1) {
  exacto <- 100 * (x1 - x0) / x0
  aproximado <- 100 * (log(x1) - log(x0))
  c(exacto = exacto, aproximado = aproximado, error = aproximado - exacto)
}

cambio(5500, 6000)     # de 5,500 a 6,000
cambio(5500, 11000)    # de 5,500 a 11,000

# Salida esperada:
#   5,500 a 6,000:   exacto=9.09,  log=8.70,  error=-0.39
#   5,500 a 11,000:  exacto=100.00, log=69.31, error=-30.69
#
# Sintesis: para el cambio chico (9.09% real) el error es de apenas 0.39
# puntos porcentuales; para el cambio grande (100% real) el error es de casi
# 31 puntos porcentuales, y la aproximacion siempre subestima los aumentos y
# sobrestima la magnitud de las caidas (porque log es concavo). Regla
# practica: usar la aproximacion hasta cambios de 10-15%; para cambios
# mayores, reportar el cambio exacto.


# =============================================================================
# Ejercicio 1.7 (estructura de datos)
# Clasificacion de cuatro conjuntos de datos hipoteticos. No requiere
# codigo ni datos: la unidad de observacion y la garantia (o no) de
# seguimiento de las mismas unidades en el tiempo alcanzan para clasificar.
#
# a) Dolar mayorista diario, 2015-2025:
#    Serie de tiempo -- una sola unidad seguida repetidamente en el tiempo.
#
# b) EPH 2T-2024, 3,000 hogares:
#    Corte transversal -- muchos hogares, un solo momento.
#
# c) PBI/poblacion/esperanza de vida, 180 paises, 1990-2000-2010-2020:
#    Panel -- mismas 180 unidades seguidas en varios momentos.
#
# d) Ventas y empleo, 500 empresas, 2019 y 2023, sin garantia de que sean
#    las mismas:
#    Corte transversal combinado -- dos momentos, pero muestras
#    independientes, no seguimiento de unidades.
# =============================================================================


# =============================================================================
# Ejercicio 1.8 (Wooldridge, problema 1.2 -- capacitacion y productividad)
# Problema conceptual, sin datos ni codigo.
#
# a) Experimento subyacente: asignar aleatoriamente la capacitacion entre
#    trabajadores/empresas identicos en todo lo demas (capital humano,
#    tecnologia, gestion, mercado) y comparar productividad ex post.
#
# b) Es independiente la decision de capacitar? No, en general. Rompen la
#    independencia: educacion previa y antiguedad (medibles); motivacion o
#    habilidad innata y calidad de gestion (no medibles).
#
# c) Factor ajeno a los trabajadores: calidad del capital fisico/tecnologia
#    de la empresa.
#
# d) La correlacion positiva establece causalidad? No: es compatible con
#    causalidad inversa (empresas mas productivas capacitan mas porque
#    tienen recursos ociosos) y con una variable omitida (calidad
#    gerencial) que afecta ambas cosas a la vez.
# =============================================================================
