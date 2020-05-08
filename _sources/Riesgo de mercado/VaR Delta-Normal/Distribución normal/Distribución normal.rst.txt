Distribución normal
-------------------

Función de Densidad de Probabilidad:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. figure:: ImagenesNormal/Formula.jpg
   :alt: 1

   1

.. figure:: ImagenesNormal/Normal1.jpg
   :alt: 2

   2

.. figure:: ImagenesNormal/Normal2.jpg
   :alt: 3

   3

Distribución normal estándar.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:math:`\mu=0` y :math:`\sigma^2=1`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. figure:: ImagenesNormal/Formula2.jpg
   :alt: 4

   4

.. figure:: ImagenesNormal/Normal3.jpg
   :alt: 5

   5

Ejemplo distribución normal estándar: :math:`\mu=0` y :math:`\sigma^2=1`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Se usa la función ``qnorm`` para hallar los valores de una distribución
normal dada una probabilidad o percentil.

Con una probabilidad del 1% z es igual a -2,32634787404084.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    z = qnorm(0.01)
    z



.. raw:: html

    -2.32634787404084


Con una probabilidad del 99% z es igual a 2,32634787404084.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    z = qnorm(0.99)
    z



.. raw:: html

    2.32634787404084


.. code:: r

    hist(1,col= "white", border = "white", xlab = "Rendimientos", ylab = "Frecuencia", main = NULL, freq = T, ylim = c(0, 0.4), xlim = c(-5,5))
    curve(dnorm(x, mean = 0, sd = 1), add = T, lwd = 3, col = "#3b5998")
    abline(v = 0, lwd = 2, lty = 1)
    abline(v = qnorm(0.01), lwd = 2, lty = 1)
    abline(v = qnorm(0.99), lwd = 2, lty = 1)



.. image:: output_14_0.png
   :width: 420px
   :height: 420px


Con una probabilidad del 5% z es igual a -1,64485362695147.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    z = qnorm(0.05)
    z



.. raw:: html

    -1.64485362695147


Con una probabilidad del 95% z es igual a 1,64485362695147.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    z = qnorm(0.95)
    z



.. raw:: html

    1.64485362695147


.. code:: r

    hist(1,col= "white", border = "white", xlab = "Rendimientos", ylab = "Frecuencia", main = NULL, freq = T, ylim = c(0, 0.4), xlim = c(-5,5))
    curve(dnorm(x, mean = 0, sd = 1), add = T, lwd = 3, col = "#3b5998")
    abline(v = 0, lwd = 2, lty = 1)
    abline(v = qnorm(0.05), lwd = 2, lty = 1)
    abline(v = qnorm(0.95), lwd = 2, lty = 1)



.. image:: output_19_0.png
   :width: 420px
   :height: 420px


Ejemplo distribución normal: :math:`\mu=1`\ % y :math:`\sigma=5`\ %
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    mu = 0.01
    volatilidad = 0.05

Con una probabilidad del 1% X es igual a -0,106317393702042.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    x = qnorm(0.01, mean = mu, sd = volatilidad)
    x



.. raw:: html

    -0.106317393702042


.. code:: r

    x = mu + qnorm(0.01, sd = volatilidad)
    x



.. raw:: html

    -0.106317393702042


Con una probabilidad del 99% X es igual a 0,126317393702042.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    x = qnorm(0.99, mean = mu, sd = volatilidad)
    x



.. raw:: html

    0.126317393702042


.. code:: r

    x = mu + qnorm(0.99, sd = volatilidad)
    x



.. raw:: html

    0.126317393702042


.. code:: r

    hist(1,col= "white", border = "white", xlab = "Rendimientos", ylab = "Frecuencia", main = NULL, freq = T, ylim = c(0, 8), xlim = c(-0.3,0.3))
    curve(dnorm(x, mean = mu, sd = volatilidad), add = T, lwd = 3, col = "#3b5998")
    abline(v = 0, lwd = 2, lty = 1)
    abline(v = qnorm(0.01, mean = mu, sd = volatilidad), lwd = 2, lty = 1)
    abline(v = qnorm(0.99, mean = mu, sd = volatilidad), lwd = 2, lty = 1)



.. image:: output_28_0.png
   :width: 420px
   :height: 420px


Ejemplo distribución normal: :math:`\mu=-1`\ % y :math:`\sigma=5`\ %
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    mu = -0.01
    volatilidad = 0.05

Con una probabilidad del 1% X es igual a -0,126317393702042.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    x = qnorm(0.01, mean = mu, sd = volatilidad)
    x



.. raw:: html

    -0.126317393702042


.. code:: r

    x = mu + qnorm(0.01, sd = volatilidad)
    x



.. raw:: html

    -0.126317393702042


Con una probabilidad del 99% X es igual a 0,106317393702042.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    x = qnorm(0.99, mean = mu, sd = volatilidad)
    x



.. raw:: html

    0.106317393702042


.. code:: r

    x = mu + qnorm(0.99, sd = volatilidad)
    x



.. raw:: html

    0.106317393702042


.. code:: r

    hist(1,col= "white", border = "white", xlab = "Rendimientos", ylab = "Frecuencia", main = NULL, freq = T, ylim = c(0, 8), xlim = c(-0.3,0.3))
    curve(dnorm(x, mean = mu, sd = volatilidad), add = T, lwd = 3, col = "#3b5998")
    abline(v = 0, lwd = 2, lty = 1)
    abline(v = qnorm(0.01, mean = mu, sd = volatilidad), lwd = 2, lty = 1)
    abline(v = qnorm(0.99, mean = mu, sd = volatilidad), lwd = 2, lty = 1)



.. image:: output_37_0.png
   :width: 420px
   :height: 420px


Importar datos.
~~~~~~~~~~~~~~~

.. code:: r

    datos = read.csv("COLCAP.csv", sep = ";", dec = ",", header = T)

Vector de puntos del COLCAP.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Los índices no son precios, son puntos.

.. code:: r

    precios = datos[,-1]

.. code:: r

    precios = ts(precios)

Vector de rendimientos.
~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    rendimientos = diff(log(precios))

:math:`\mu:` Rendimiento esperado del índice.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    rendimiento_esperado = mean(rendimientos)
    rendimiento_esperado



.. raw:: html

    -0.000252769425608668


:math:`\sigma:`\ Volatilidad del índice.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    volatilidad = sd(rendimientos)
    volatilidad



.. raw:: html

    0.0138522072303189


Gráfico: puntos del COLCAP.
~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    plot(precios, xlab = "Tiempo", ylab = "Puntos", col = "#3b5998", lwd = 3)



.. image:: output_51_0.png
   :width: 420px
   :height: 420px


Gráfico: rendimientos.
~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    plot(rendimientos, t = "h", xlab = "Tiempo", ylab = "Rendimientos", col = "#3b5998", lwd = 2)



.. image:: output_53_0.png
   :width: 420px
   :height: 420px


Gráfico: histograma de los rendimientos
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    hist(rendimientos, breaks = 60, col= "#3b5998", xlab = "Rendimientos", ylab = "Frecuencia", main = "Índice COLCAP", freq = F)



.. image:: output_55_0.png
   :width: 420px
   :height: 420px


Gráfico: Histograma y distribución normal con :math:`\mu=0`.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    hist(rendimientos, breaks = 60, col= "gray", border = "white", xlab = "Rendimientos", ylab = "Frecuencia", main = "Índice COLCAP", freq = F)
    curve(dnorm(x, mean = 0, sd = volatilidad*sqrt(1)), add = T, lwd = 3, col = "#3b5998")



.. image:: output_57_0.png
   :width: 420px
   :height: 420px


Gráfico: distribución normal con :math:`\mu=0`.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    hist(rendimientos,col= "white", border = "white", xlab = "Rendimientos", ylab = "Frecuencia", main = NULL, freq = F, ylim = c(0, 30))
    curve(dnorm(x, mean = 0, sd = volatilidad*sqrt(1)), add = T, lwd = 3, col = "#3b5998")
    curve(dnorm(x, mean = 0, sd = volatilidad*sqrt(5)), add = T, lwd = 3, col = "firebrick3")
    curve(dnorm(x, mean = 0, sd = volatilidad*sqrt(10)), add = T, lwd = 3, col = "forestgreen")
    curve(dnorm(x, mean = 0, sd = volatilidad*sqrt(20)), add = T, lwd = 3)
    legend("topright", c("Diaria", "Semanal", "15 días", "Mensual"), col = c("#3b5998", "firebrick3", "forestgreen", "black"), lty = c(1,1,1,1), lwd = 3, bty = "n")



.. image:: output_59_0.png
   :width: 420px
   :height: 420px


Gráfico: distribución normal con :math:`\mu=0` y percentiles.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    hist(rendimientos,col= "white", border = "white", xlab = "Rendimientos", ylab = "Frecuencia", main = NULL, freq = F, ylim = c(0, 30))
    curve(dnorm(x, mean = 0, sd = volatilidad*sqrt(1)), add = T, lwd = 3, col = "#3b5998")
    curve(dnorm(x, mean = 0, sd = volatilidad*sqrt(5)), add = T, lwd = 3, col = "firebrick3")
    curve(dnorm(x, mean = 0, sd = volatilidad*sqrt(10)), add = T, lwd = 3, col = "forestgreen")
    curve(dnorm(x, mean = 0, sd = volatilidad*sqrt(20)), add = T, lwd = 3)
    abline(v = -volatilidad*qnorm(0.99)*sqrt(1), lwd = 3, col = "#3b5998")
    abline(v = -volatilidad*qnorm(0.99)*sqrt(5), lwd = 3, col = "firebrick3")
    abline(v = -volatilidad*qnorm(0.99)*sqrt(10), lwd = 3, col = "forestgreen")
    abline(v = -volatilidad*qnorm(0.99)*sqrt(20), lwd = 3)
    legend("topright", c("Diaria", "Semanal", "15 días", "Mensual"), col = c("#3b5998", "firebrick3", "forestgreen", "black"), lty = c(1,1,1,1), lwd = 3, bty = "n")



.. image:: output_61_0.png
   :width: 420px
   :height: 420px


Gráfico: distribución normal con :math:`\mu\neq0`.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    hist(rendimientos,col= "white", border = "white", xlab = "Rendimientos", ylab = "Frecuencia", main = NULL, freq = F, ylim = c(0, 30))
    curve(dnorm(x, mean = rendimiento_esperado*1, sd = volatilidad*sqrt(1)), add = T, lwd = 3, col = "#3b5998")
    curve(dnorm(x, mean = rendimiento_esperado*5, sd = volatilidad*sqrt(5)), add = T, lwd = 3, col = "firebrick3")
    curve(dnorm(x, mean = rendimiento_esperado*10, sd = volatilidad*sqrt(10)), add = T, lwd = 3, col = "forestgreen")
    curve(dnorm(x, mean = rendimiento_esperado*20, sd = volatilidad*sqrt(20)), add = T, lwd = 3)
    abline(v = rendimiento_esperado*20, lwd = 3, lty = 1)
    legend("topright", c("Diaria", "Semanal", "15 días", "Mensual"), col = c("#3b5998", "firebrick3", "forestgreen", "black"), lty = c(1,1,1,1), lwd = 3, bty = "n")



.. image:: output_63_0.png
   :width: 420px
   :height: 420px

