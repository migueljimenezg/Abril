VaR método Delta-Normal o varianzas-covarianzas
-----------------------------------------------

Importar datos.
~~~~~~~~~~~~~~~

.. code:: r

    datos = read.csv("Cuatro acciones 2020.csv", sep = ";", dec = ",", header = T)

Matriz de precios.
~~~~~~~~~~~~~~~~~~

Crear una matriz con los valores de los precios de las acciones:
``[,-1]`` elimina la primera columna de la matriz ``datos`` que
corresponde a las fechas. Quedan solo los precios.

.. code:: r

    precios = datos[,-1]

.. code:: r

    head(precios)



.. raw:: html

    <table>
    <caption>A data.frame: 6 × 4</caption>
    <thead>
    	<tr><th></th><th scope=col>ECO</th><th scope=col>PFAVAL</th><th scope=col>ISA</th><th scope=col>NUTRESA</th></tr>
    	<tr><th></th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th></tr>
    </thead>
    <tbody>
    	<tr><th scope=row>1</th><td>2775</td><td>1165</td><td>13080</td><td>25720</td></tr>
    	<tr><th scope=row>2</th><td>2645</td><td>1155</td><td>13080</td><td>25700</td></tr>
    	<tr><th scope=row>3</th><td>2615</td><td>1165</td><td>13320</td><td>25980</td></tr>
    	<tr><th scope=row>4</th><td>2690</td><td>1165</td><td>13420</td><td>25920</td></tr>
    	<tr><th scope=row>5</th><td>2730</td><td>1175</td><td>13660</td><td>25920</td></tr>
    	<tr><th scope=row>6</th><td>2740</td><td>1190</td><td>13560</td><td>25840</td></tr>
    </tbody>
    </table>
    


.. code:: r

    precios = ts(precios)

.. code:: r

    dim(precios)



.. raw:: html

    <style>
    .list-inline {list-style: none; margin:0; padding: 0}
    .list-inline>li {display: inline-block}
    .list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
    </style>
    <ol class=list-inline><li>500</li><li>4</li></ol>
    


Se cargaron cuatro acciones y 500 precios por acción.

Nombres de las acciones.
~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    nombres = colnames(precios)
    nombres



.. raw:: html

    <style>
    .list-inline {list-style: none; margin:0; padding: 0}
    .list-inline>li {display: inline-block}
    .list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
    </style>
    <ol class=list-inline><li>'ECO'</li><li>'PFAVAL'</li><li>'ISA'</li><li>'NUTRESA'</li></ol>
    


Número de acciones.
~~~~~~~~~~~~~~~~~~~

.. code:: r

    acciones = ncol(precios)
    acciones



.. raw:: html

    4


Matriz de rendimientos.
~~~~~~~~~~~~~~~~~~~~~~~

.. math:: Rendimiento_t=log\frac{Precio_t}{Precio_{t-1}}

.. math:: log\frac{Precio_t}{Precio_{t-1}}=log(Precio_t)-log(Precio_{t-1})

.. code:: r

    rendimientos = diff(log(precios))

.. code:: r

    dim(rendimientos)



.. raw:: html

    <style>
    .list-inline {list-style: none; margin:0; padding: 0}
    .list-inline>li {display: inline-block}
    .list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
    </style>
    <ol class=list-inline><li>499</li><li>4</li></ol>
    


Hay 499 rendimientos por acción.

:math:`S_0:`\ Precio actual de cada acción.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    s = tail(precios,1)
    s = as.numeric(s)
    s



.. raw:: html

    <style>
    .list-inline {list-style: none; margin:0; padding: 0}
    .list-inline>li {display: inline-block}
    .list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
    </style>
    <ol class=list-inline><li>2220</li><li>955</li><li>18000</li><li>22500</li></ol>
    


El precio de ECO es de 2.220 COP, de PFAVAL 955 COP, ISA de 18.000 COP y
NUTRESA de 22.500 COP.

Número de acciones del portafolio de inversión.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    numero_acciones = c(180000,5000,12000,9000)
    numero_acciones



.. raw:: html

    <style>
    .list-inline {list-style: none; margin:0; padding: 0}
    .list-inline>li {display: inline-block}
    .list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
    </style>
    <ol class=list-inline><li>180000</li><li>5000</li><li>12000</li><li>9000</li></ol>
    


Valor de mercado de cada acción.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    valor_mercado_acciones = numero_acciones*s
    valor_mercado_acciones



.. raw:: html

    <style>
    .list-inline {list-style: none; margin:0; padding: 0}
    .list-inline>li {display: inline-block}
    .list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
    </style>
    <ol class=list-inline><li>399600000</li><li>4775000</li><li>2.16e+08</li><li>202500000</li></ol>
    


**ECO:** $ 399.600.000.

**PFAVAL:** $ 4.775.000.

**ISA:** $ 216.000.000.

**NUTRESA:** $ 202.500.000.

Valor de mercado del portafolio de inversión.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    valor_portafolio = sum(valor_mercado_acciones)
    valor_portafolio



.. raw:: html

    822875000


El portafolio de inversión está valorado en $ 822.875.000.

Proporciones de inversión.
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    proporciones = valor_mercado_acciones/valor_portafolio
    proporciones



.. raw:: html

    <style>
    .list-inline {list-style: none; margin:0; padding: 0}
    .list-inline>li {display: inline-block}
    .list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
    </style>
    <ol class=list-inline><li>0.485614461491721</li><li>0.00580282545951694</li><li>0.262494303509038</li><li>0.246088409539724</li></ol>
    


**ECO:** 48,56%.

**PFAVAL:** 0,580%.

**ISA:** 26,25%.

**NUTRESA:** 24,61%.

.. math::  \sum_{i=1}^nw_i=1

.. code:: r

    sum(proporciones)



.. raw:: html

    1


:math:`\mu:` Rendimiento esperado de cada acción.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    rendimientos_esperados = apply(rendimientos,2,mean)
    rendimientos_esperados



.. raw:: html

    <style>
    .dl-inline {width: auto; margin:0; padding: 0}
    .dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
    .dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
    .dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
    </style><dl class=dl-inline><dt>ECO</dt><dd>-0.000447181465559539</dd><dt>PFAVAL</dt><dd>-0.000398326704447035</dd><dt>ISA</dt><dd>0.000639854532799824</dd><dt>NUTRESA</dt><dd>-0.000268043266851791</dd></dl>
    


**ECO:** -0,0447% diario.

**PFAVAL:** -0,0398% diario.

**ISA:** 0,0640% diario.

**NUTRESA:** -0,0268% diario.

:math:`\sigma:`\ Volatilidad de cada acción.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    volatilidades = apply(rendimientos,2,sd)
    volatilidades



.. raw:: html

    <style>
    .dl-inline {width: auto; margin:0; padding: 0}
    .dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
    .dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
    .dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
    </style><dl class=dl-inline><dt>ECO</dt><dd>0.0319324424190137</dd><dt>PFAVAL</dt><dd>0.0285577211893029</dd><dt>ISA</dt><dd>0.0237292026947701</dd><dt>NUTRESA</dt><dd>0.0140104740592151</dd></dl>
    


**ECO:** 3,193% diaria.

**PFAVAL:** 2,856% diaria.

**ISA:** 2,373% diaria.

**NUTRESA:** 1,401% diaria.

Matriz varianzas-covarianzas.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    covarianzas = cov(rendimientos)
    covarianzas



.. raw:: html

    <table>
    <caption>A matrix: 4 × 4 of type dbl</caption>
    <thead>
    	<tr><th></th><th scope=col>ECO</th><th scope=col>PFAVAL</th><th scope=col>ISA</th><th scope=col>NUTRESA</th></tr>
    </thead>
    <tbody>
    	<tr><th scope=row>ECO</th><td>0.0010196809</td><td>0.0005939468</td><td>0.0001160327</td><td>0.0001493216</td></tr>
    	<tr><th scope=row>PFAVAL</th><td>0.0005939468</td><td>0.0008155434</td><td>0.0001564360</td><td>0.0001322689</td></tr>
    	<tr><th scope=row>ISA</th><td>0.0001160327</td><td>0.0001564360</td><td>0.0005630751</td><td>0.0001519996</td></tr>
    	<tr><th scope=row>NUTRESA</th><td>0.0001493216</td><td>0.0001322689</td><td>0.0001519996</td><td>0.0001962934</td></tr>
    </tbody>
    </table>
    


Coeficientes de correlación.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    correlacion = cor(rendimientos)
    correlacion



.. raw:: html

    <table>
    <caption>A matrix: 4 × 4 of type dbl</caption>
    <thead>
    	<tr><th></th><th scope=col>ECO</th><th scope=col>PFAVAL</th><th scope=col>ISA</th><th scope=col>NUTRESA</th></tr>
    </thead>
    <tbody>
    	<tr><th scope=row>ECO</th><td>1.0000000</td><td>0.6513161</td><td>0.1531317</td><td>0.3337626</td></tr>
    	<tr><th scope=row>PFAVAL</th><td>0.6513161</td><td>1.0000000</td><td>0.2308501</td><td>0.3305836</td></tr>
    	<tr><th scope=row>ISA</th><td>0.1531317</td><td>0.2308501</td><td>1.0000000</td><td>0.4572004</td></tr>
    	<tr><th scope=row>NUTRESA</th><td>0.3337626</td><td>0.3305836</td><td>0.4572004</td><td>1.0000000</td></tr>
    </tbody>
    </table>
    


Rendimientos del portafolio de inversión.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. math::  R_P=\sum_{i=1}^nw_iR_i

---------------------------------

.. code:: r

    rendimientos_portafolio = vector()
    
    for(i in 1:nrow(rendimientos)){
        
      rendimientos_portafolio[i] = sum(rendimientos[i,]*proporciones)
      
    }

Rendimiento esperado del portafolio de inversión.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    rendimiento_esperado_portafolio = mean(rendimientos_portafolio)
    rendimiento_esperado_portafolio



.. raw:: html

    -0.000117473378221543


Rendimientos esperado del portafolio de inversión diario de -0,012%.

Volatilidad del portafolio de inversión.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    volatilidad_portafolio = sd(rendimientos_portafolio)
    volatilidad_portafolio



.. raw:: html

    0.0195008972788864


Volatilidad del portafolio de inversión diaria de 1,950%.

Volatilidad del portafolio a partir de la matriz de varianzas-covarianzas.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    volatilidad_portafolio = sqrt(sum(t(proporciones)%*%covarianzas*proporciones))
    volatilidad_portafolio



.. raw:: html

    0.0195008972788864


Volatilidad del portafolio de inversión diaria de 1,950%.

VaR (sin promedios)
~~~~~~~~~~~~~~~~~~~

Nivel de confianza, ``NC``, del 99% y horizonte de tiempo, ``t``, de 10
días. Estos días representan dos semanas.

La frecuencia de los datos es diaria, por tanto ``t = 10``.

.. code:: r

    NC = 0.99
    t = 10

.. math::  \alpha=0,01

----------------------

.. figure:: VaR1.jpg
   :alt: 1

   1

En la distribución normal con :math:`\mu=0`, se cumple que:

.. math::  |Z_{\alpha=0,01}|=Z_{NC=0,99}

----------------------------------------

VaR individuales (sin promedios) [%].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. math::  VaR=Z_{\alpha}\sigma\sqrt{(t)}

=========================================

.. code:: r

    VaR_individuales_sin_promedios_porcentaje = volatilidades*qnorm(NC)*sqrt(t)
    VaR_individuales_sin_promedios_porcentaje



.. raw:: html

    <style>
    .dl-inline {width: auto; margin:0; padding: 0}
    .dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
    .dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
    .dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
    </style><dl class=dl-inline><dt>ECO</dt><dd>0.234912861922595</dd><dt>PFAVAL</dt><dd>0.210086529759846</dd><dt>ISA</dt><dd>0.17456525382633</dd><dt>NUTRESA</dt><dd>0.103068863789222</dd></dl>
    


-  Con un nivel de confianza del 99%, el VaR diario de ECO es de 23,49%.

-  Con una probabilidad del 99%, se espera perder 21,01% o menos cada
   día con la acción de PFAVAL.

-  Con una probabilidad del 1%, se espera perder más de 17,46% cada día
   con la acción de ISA.

-  De cada 100 días, se espera que en un (1) día se pierda más de 10,31%
   con la acción de NUTRESA y 99 días perder 10,31% o menos.

VaR individuales (sin promedios) [$].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. math::  VaR=Z_{\alpha}\sigma V_0\sqrt{(t)}

=============================================

.. code:: r

    VaR_individuales_sin_promedios = valor_mercado_acciones*volatilidades*qnorm(NC)*sqrt(t)
    VaR_individuales_sin_promedios



.. raw:: html

    <style>
    .dl-inline {width: auto; margin:0; padding: 0}
    .dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
    .dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
    .dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
    </style><dl class=dl-inline><dt>ECO</dt><dd>93871179.624269</dd><dt>PFAVAL</dt><dd>1003163.17960326</dd><dt>ISA</dt><dd>37706094.8264872</dd><dt>NUTRESA</dt><dd>20871444.9173174</dd></dl>
    


Conclusión:
~~~~~~~~~~~

-  Con un nivel de confianza del 99%, el VaR diario de ECO es de
   93.871.179,62 COP.

-  Con una probabilidad del 99%, se espera perder 1.003.163,18 COP o
   menos cada día con la acción de PFAVAL.

-  Con una probabilidad del 1%, se espera perder más de 37.706.094,83
   COP cada día con la acción de ISA.

-  De cada 100 días, se espera que en un (1) día se pierda más de
   20.871.444,92 COP con la acción de NUTRESA y 99 días perder
   20.871.444,92 COP o menos.

El vector ``VaR_individuales_sin_promedios`` está vertical.

VaR portafolio de inversión (sin promedios) [$].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. math::  VaR_P=\sqrt{\sum_{i=1}^nVaR_i^2+2\sum_{i<j}VaR_iVaR_j\rho_{i,j}}

---------------------------------------------------------------------------

.. figure:: VaR2.jpg
   :alt: 2

   2

.. math:: VaR_P=\sqrt{[VaR]C[VaR]^T}

------------------------------------

Se transpone el vector ``VaR_individuales_sin_promedios`` al principio
de la ecuación porque está vertical y debe ser primero horizontal. Al
final de la ecuación debe estar vertical.

Para transponer se usa la función ``t``.

:math:`C:` Matríz de correlaciones.

:math:`VaR:` vector fila de los VaR individuales.

:math:`VaR^T:` vector de los VaR individuales transpuesto.

.. code:: r

    VaR_portafolio_sin_promedios = sqrt(sum(t(VaR_individuales_sin_promedios)%*%correlacion*VaR_individuales_sin_promedios))
    VaR_portafolio_sin_promedios



.. raw:: html

    118049219.741064


Conclusión:
~~~~~~~~~~~

-  Con un nivel de confianza del 99%, el VaR diario del portafolio de
   inversión es de 118.049.219,74 COP.

-  Con una probabilidad del 99%, se espera perder 118.049.219,74 COP o
   menos cada día en el portafolio de inversión.

-  Con una probabilidad del 1%, se espera perder más de 118.049.219,74
   COP cada día en el portafolio de inversión.

-  De cada 100 días, se espera que en un (1) día se pierda más de
   20.871.444,92 COP con la acción de NUTRESA y 99 días perder
   20.871.444,92 COP o menos.

VaR portafolio de inversión (sin promedios) [%].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    VaR_portafolio_sin_promedios_porcentaje = VaR_portafolio_sin_promedios/valor_portafolio
    VaR_portafolio_sin_promedios_porcentaje



.. raw:: html

    0.143459480165352


Conclusión:
~~~~~~~~~~~

-  Con un nivel de confianza del 99%, el VaR diario del portafolio de
   inversión es de 14,35%.

-  Con una probabilidad del 99%, se espera perder 14,35% o menos cada
   día en el portafolio de inversión.

-  Con una probabilidad del 1%, se espera perder más de 14,35% cada día
   en el portafolio de inversión.

-  De cada 100 días, se espera que en un (1) día se pierda más de
   20.871.444,92 COP con la acción de NUTRESA y 99 días perder
   20.871.444,92 COP o menos.

Sumatoria de los VaR.
~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    suma_VaR_individuales_sin_promedios = sum(VaR_individuales_sin_promedios)
    suma_VaR_individuales_sin_promedios



.. raw:: html

    153451882.547677


Suponiendo que las acciones no conforman un portafolio de inversión, la
sumatoria de los VaR individuales es de 153.451.882,54 COP; sin embargo,
como las acciones están en un portafolio de inversión, el VaR del
portafolio de inversión con un nivel de confianza del 99% es de
118.049.219,74 COP.

La diferencia entre los VaR individuales sumados y el VaR resultante del
portafolio de inversión se conoce como Beneficio por diversificación
(BD).

Beneficio por diversificación (BD)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

El VaR global es menor o igual que la suma de los VaR individuales.

.. math::  VaR \leq \sum_{i=1}^nVaR_i

-------------------------------------

.. math::  BD = \sum_{i=1}^n VaR_i - VaR_P

------------------------------------------

.. code:: r

    BD_sin_promedios = suma_VaR_individuales_sin_promedios - VaR_portafolio_sin_promedios
    BD_sin_promedios



.. raw:: html

    35402662.8066126


El Beneficio por diversificación (BD) indica cuánto es el beneficio por
diversificar el portafolio de inversión con las acciones.

Con un nivel de confianza del 99%, el BD diario es de 35.402.662,81 COP.
Cada día dejaría de perder 35.402.662,81 COP en el portafolio de
inversión.

VaR (con promedios)
~~~~~~~~~~~~~~~~~~~

En la distribución normal con :math:`\mu\neq0`:

.. math::  |Z_{\alpha=0.01}|\neq Z_{NC=0.99}

--------------------------------------------

Nivel de confianza, ``NC``, del 99% y horizonte de tiempo, ``t``, de 10
días. Estos días representan dos semanas.

La frecuencia de los datos es diaria, por tanto ``t = 10``.

.. code:: r

    NC = 0.99
    t = 10

VaR individuales (con promedios) [%].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. math::  VaR = |\mu + Z_{\alpha} \sigma|

------------------------------------------

.. code:: r

    VaR_individuales_con_promedios_porcentaje = abs(rendimientos_esperados*t+qnorm(1-NC, sd = volatilidades*sqrt(t)))
    VaR_individuales_con_promedios_porcentaje



.. raw:: html

    <style>
    .dl-inline {width: auto; margin:0; padding: 0}
    .dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
    .dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
    .dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
    </style><dl class=dl-inline><dt>ECO</dt><dd>0.23938467657819</dd><dt>PFAVAL</dt><dd>0.214069796804316</dd><dt>ISA</dt><dd>0.168166708498332</dd><dt>NUTRESA</dt><dd>0.10574929645774</dd></dl>
    


VaR individuales (con promedios) [$].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. math::  VaR = |\mu + Z_{\alpha} \sigma V_0|

----------------------------------------------

.. code:: r

    VaR_individuales_con_promedios = valor_mercado_acciones*abs(rendimientos_esperados*t+qnorm(1-NC, sd = volatilidades*sqrt(t)))
    VaR_individuales_con_promedios



.. raw:: html

    <style>
    .dl-inline {width: auto; margin:0; padding: 0}
    .dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
    .dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
    .dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
    </style><dl class=dl-inline><dt>ECO</dt><dd>95658116.7606449</dd><dt>PFAVAL</dt><dd>1022183.27974061</dd><dt>ISA</dt><dd>36324009.0356396</dd><dt>NUTRESA</dt><dd>21414232.5326923</dd></dl>
    


VaR portafolio de inversión (con promedios) [$].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Se transpone el vector ``VaR_individuales_con_promedios`` al principio
de la ecuación porque está vertical y debe ser primero horizontal. Al
final de la ecuación debe estar vertical.

Para transponer se usa la función ``t``.

.. code:: r

    VaR_portafolio_con_promedios = sqrt(sum(t(VaR_individuales_con_promedios)%*%correlacion*VaR_individuales_con_promedios))
    VaR_portafolio_con_promedios



.. raw:: html

    119295160.239381


VaR portafolio de inversión (con promedios) [%].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    VaR_portafolio_con_promedios_porcentaje = VaR_portafolio_con_promedios/valor_portafolio
    VaR_portafolio_con_promedios_porcentaje



.. raw:: html

    0.144973611106646


Sumatoria de los VaR.
~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    suma_VaR_individuales_con_promedios = sum(VaR_individuales_con_promedios)
    suma_VaR_individuales_con_promedios



.. raw:: html

    154418541.608717


Beneficio por diversificación (BD)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    BD_con_promedios = suma_VaR_individuales_con_promedios - VaR_portafolio_con_promedios
    BD_con_promedios



.. raw:: html

    35123381.3693362


Otra forma de calcular el VaR (sin promedios)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Esta forma es a partir de las proporciones de inversión,
``proporciones``, y la matriz varianzas-covarianzas, ``covarianzas``.

El vector ``proporciones`` está vertical, se debe transponer al
principio de la ecuación.

Volatilidad del portafolio a partir de la matriz de varianzas-covarianzas
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    vol_portafolio = sqrt(sum(t(proporciones)%*%covarianzas*proporciones))
    vol_portafolio



.. raw:: html

    0.0195008972788864


VaR portafolio de inversión (sin promedios) [$].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    VaR_portafolio_sin_promedios = valor_portafolio*vol_portafolio*qnorm(NC)*sqrt(t)
    VaR_portafolio_sin_promedios



.. raw:: html

    118049219.741064


Este valor es igual al calculado anteriormente a partir de los VaR
individuales (sin promedio).

Gráficos
~~~~~~~~

Precios de las acciones.
~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    plot(precios, col = "darkblue", lwd = 2, main = "Precios")



.. image:: output_123_0.png
   :width: 420px
   :height: 420px


Rendimientos de las acciones.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    plot(rendimientos, col = "darkblue", lwd = 2, main = "Rendimientos")



.. image:: output_125_0.png
   :width: 420px
   :height: 420px


Histograma, distribución normal y VaR (sin promedios) de las acciones.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    layout(matrix(c(1:4), nrow = 2, byrow = T))
    # layout.show(4)  correr esta línea en RStudio.
    
    for(i in 1:acciones){
      
      hist(rendimientos[,i], breaks = 60, col= "gray", xlab = "Rendimientos", ylab = "Frecuencia", main = nombres[i], freq = F, xlim = c(-0.5, 0.5))
      curve(dnorm(x, mean = 0, sd = volatilidades[i]*sqrt(t)), add = T, lwd = 3)
      abline(v = - VaR_individuales_sin_promedios_porcentaje[i], col = "darkred", lwd = 2)
        
    }



.. image:: output_127_0.png
   :width: 420px
   :height: 420px


Histograma, distribución normal y VaR (sin promedios) del portafolio de inversión.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    hist(rendimientos_portafolio, breaks = 60, col = "gray", xlab = "Rendimientos", ylab = "Frecuencia", main = "Portafolio de inversión", freq=F, xlim = c(-0.3, 0.3))
    curve(dnorm(x, mean = 0, sd = volatilidad_portafolio*sqrt(t)), add = T, lwd = 3)
    abline(v = -VaR_portafolio_con_promedios_porcentaje, col = "darkgreen", lwd = 4)



.. image:: output_129_0.png
   :width: 420px
   :height: 420px


Comparación VaR (sin promedios).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    hist(rendimientos_portafolio, breaks = 60, col = "gray", xlab = "Rendimientos", ylab = "Frecuencia", main = "", freq=F, xlim = c(-0.3,0.1))
    for(i in 1:acciones){
        
    abline(v = - VaR_individuales_sin_promedios_porcentaje[i], col = i, lwd = 2)
        
    }
    abline(v = -VaR_portafolio_sin_promedios_porcentaje, col = "darkgreen", lwd = 4)
    legend("topleft", nombres, lty = rep(1, times = acciones), lwd = rep(2, times = acciones), col = seq(1, acciones), bty = "n")



.. image:: output_131_0.png
   :width: 420px
   :height: 420px


Histograma, distribución normal y VaR (con promedios) de las acciones.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    layout(matrix(c(1:4), nrow = 2, byrow = T))
    # layout.show(4)  correr esta línea en RStudio.
    
    for(i in 1:acciones){
      
      hist(rendimientos[,i], breaks = 60, col= "gray", xlab = "Rendimientos", ylab = "Frecuencia", main = nombres[i], freq = F, xlim = c(-0.5, 0.5))
      curve(dnorm(x, mean = rendimientos_esperados[i]*t, sd = volatilidades[i]*sqrt(t)), add = T, lwd = 3)
      abline(v = - VaR_individuales_con_promedios_porcentaje[i], col = "darkred", lwd = 2)
        
    }



.. image:: output_133_0.png
   :width: 420px
   :height: 420px


Histograma, distribución normal y VaR (con promedios) del portafolio de inversión.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    hist(rendimientos_portafolio, breaks = 60, col = "gray", xlab = "Rendimientos", ylab = "Frecuencia", main = "Portafolio de inversión", freq=F, xlim = c(-0.5, 0.5))
    curve(dnorm(x, mean = rendimiento_esperado_portafolio*t, sd = volatilidad_portafolio*sqrt(t)), add = T, lwd = 3)
    abline(v = -VaR_portafolio_con_promedios_porcentaje, col = "darkgreen", lwd = 4)



.. image:: output_135_0.png
   :width: 420px
   :height: 420px


Comparación VaR (con promedios).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    hist(rendimientos_portafolio, breaks = 60, col = "gray", xlab = "Rendimientos", ylab = "Frecuencia", main = "", freq=F, xlim = c(-0.3,0.1))
    for(i in 1:acciones){
        
    abline(v = - VaR_individuales_con_promedios_porcentaje[i], col = i, lwd = 2)
        
    }
    abline(v = -VaR_portafolio_con_promedios_porcentaje, col = "darkgreen", lwd = 4)
    legend("topleft", nombres, lty = rep(1, times = acciones), lwd = rep(2, times = acciones), col = seq(1, acciones), bty = "n")



.. image:: output_137_0.png
   :width: 420px
   :height: 420px

