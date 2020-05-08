Taller N° 2: VaR método Delta-Normal o varianzas-covarianzas
------------------------------------------------------------

Importar datos.
~~~~~~~~~~~~~~~

.. code:: r

    datos = read.csv("TRM.csv", sep = ";", dec = ",", header = T)

Vector de precios.
~~~~~~~~~~~~~~~~~~

.. code:: r

    precios = datos[,-1]

.. code:: r

    precios = ts(precios)

:math:`S_0:`\ Precio actual de la TRM.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    s = tail(precios,1)
    s = as.numeric(s)
    s



.. raw:: html

    3401.56


Vector de rendimientos.
~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    rendimientos = diff(log(precios))

:math:`\mu:` Rendimiento esperado de la TRM.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    rendimiento_esperado = mean(rendimientos)
    rendimiento_esperado



.. raw:: html

    0.000297788962306605


:math:`\sigma:`\ Volatilidad de la TRM.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    volatilidad = sd(rendimientos)
    volatilidad



.. raw:: html

    0.00613174656540523


Gráficos
~~~~~~~~

Precio de la TRM
~~~~~~~~~~~~~~~~

.. code:: r

    plot(precios, col = "darkblue", lwd = 2, main = "Precios")



.. image:: output_16_0.png
   :width: 420px
   :height: 420px


Rendimientos de la TRM
~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    plot(rendimientos, col = "darkblue", lwd = 2, main = "Rendimientos")



.. image:: output_18_0.png
   :width: 420px
   :height: 420px


Histograma y distribución normal.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

La distribución normal se dibuja con una frecuencia diaria.

.. code:: r

    hist(rendimientos, breaks = 40, col= "gray", border = "gray", xlab = "Rendimientos", ylab = "Frecuencia", main = "TRM", freq = F)
    curve(dnorm(x, mean = 0, sd = volatilidad), add = T, lwd = 4)



.. image:: output_21_0.png
   :width: 420px
   :height: 420px


VaR (sin promedios)
~~~~~~~~~~~~~~~~~~~

.. code:: r

    NC = 0.99
    t = 10

VaR individuales (sin promedios) [%].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    VaR_sin_promedios_porcentaje = volatilidad*qnorm(NC)*sqrt(t)
    VaR_sin_promedios_porcentaje



.. raw:: html

    0.0451085487092495


VaR individuales (sin promedios) [$].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    VaR_sin_promedios = s*volatilidad*qnorm(NC)*sqrt(t)
    VaR_sin_promedios



.. raw:: html

    153.439434947435


VaR (con promedios)
~~~~~~~~~~~~~~~~~~~

.. code:: r

    NC = 0.99
    t = 10

VaR individuales (con promedios) [%].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    VaR_con_promedios_porcentaje = abs(rendimiento_esperado*t+qnorm(1-NC,sd=volatilidad*sqrt(t)))
    VaR_con_promedios_porcentaje



.. raw:: html

    0.0421306590861834


VaR individuales (con promedios) [$].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    VaR_con_promedios = s*abs(rendimiento_esperado*t+qnorm(1-NC,sd=volatilidad*sqrt(t)))
    VaR_con_promedios



.. raw:: html

    143.309964721198


Histograma, distribución normal y VaR (sin promedios) de la TRM.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

La distribución normal se dibuja con una frecuencia de 10 días.

.. code:: r

    hist(rendimientos, breaks = 40, col= "gray", border = "white", xlab = "Rendimientos", ylab = "Frecuencia", main = "TRM", freq = F, xlim = c(-0.055, 0.055))
    curve(dnorm(x, mean = 0, sd = volatilidad*sqrt(t)), add = T, lwd = 3)
    abline(v = - VaR_sin_promedios_porcentaje, col = "darkblue", lwd = 3)



.. image:: output_36_0.png
   :width: 420px
   :height: 420px


Histograma, distribución normal y VaR (con promedios) de la TRM.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

La distribución normal se dibuja con una frecuencia de 10 días.

.. code:: r

    hist(rendimientos, breaks = 40, col= "gray", border = "white", xlab = "Rendimientos", ylab = "Frecuencia", main = "TRM", freq = F, xlim = c(-0.055, 0.055))
    curve(dnorm(x, mean = rendimiento_esperado*t, sd = volatilidad*sqrt(t)), add = T, lwd = 3)
    abline(v = - VaR_con_promedios_porcentaje, col = "darkgreen", lwd = 3)



.. image:: output_39_0.png
   :width: 420px
   :height: 420px


Comparación VaR
~~~~~~~~~~~~~~~

La distribuciones normal se dibujan con una frecuencia de 10 días.

.. code:: r

    hist(rendimientos, breaks = 40, col= "gray", border = "white", xlab = "Rendimientos", ylab = "Frecuencia", main = "TRM", freq = F, xlim = c(-0.055, 0.055))
    curve(dnorm(x, mean = 0, sd = volatilidad*sqrt(t)), add = T, lwd = 3, col = "darkred")
    curve(dnorm(x, mean = rendimiento_esperado*t, sd = volatilidad*sqrt(t)), add = T, lwd = 3)
    abline(v = - VaR_sin_promedios_porcentaje, col = "darkblue", lwd = 3)
    abline(v = - VaR_con_promedios_porcentaje, col = "darkgreen", lwd = 3)
    legend("topright", c("VaR sin promedios", "VaR con promedios", "Normal media cero", "Normal"), lty = c(1,1,1,1), lwd = 3, col = c("darkblue", "darkgreen", "darkred", "black"), bty = "n")



.. image:: output_42_0.png
   :width: 420px
   :height: 420px

