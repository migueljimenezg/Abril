Extraer datos de Yahoo Finance
------------------------------

Existen códigos en R que extraen información de algunas páginas web. Los
paquetes ``quantmod`` y ``tseries`` nos ayudan a descargar los
históricos de los precios de las acciones directamente desde la página
de *Yahoo Finance.*

Estos paquetes se deben instalar de la siguiente manera:

``install.packages('quantmod')``

``install.packages("tseries")``

Para usar las librerías de estos paquetes debemos activarlos en R de la
siguiente forma:

``library(quantmod)``

``library(tseries)``

``getSymbols`` para descargar precios de acciones
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    library(quantmod)

Dentro de esta librería se usa la función ``getSymbols``.

Descargar precios con fechas específicas
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

El siguiente ejemplo extrae los precios de las acciones de Netflix y
Apple desde el primero de enero de 2015 hasta el 14 de abril de 2020.
Note que descarga hasta un día bursátil antes del indicado, que fue el
15 de abril de 2020.

.. code:: r

    getSymbols(c("NFLX","AAPL"), from = "2015-01-01", to = "2020-04-15")



.. raw:: html

    <style>
    .list-inline {list-style: none; margin:0; padding: 0}
    .list-inline>li {display: inline-block}
    .list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
    </style>
    <ol class=list-inline><li>'NFLX'</li><li>'AAPL'</li></ol>
    


El resultado del código anterior muestra que existen dos objetos que
almacenan los precios de las acciones descargadas. Estos objetos tienen
el nombre de ``NFLX`` y ``AAPL``, que son los nemotécnicos de las
acciones.

.. code:: r

    head(NFLX)



.. parsed-literal::

               NFLX.Open NFLX.High NFLX.Low NFLX.Close NFLX.Volume NFLX.Adjusted
    2015-01-02  49.15143  50.33143 48.73143   49.84857    13475000      49.84857
    2015-01-05  49.25857  49.25857 47.14714   47.31143    18165000      47.31143
    2015-01-06  47.34714  47.64000 45.66143   46.50143    16037700      46.50143
    2015-01-07  47.34714  47.42143 46.27143   46.74286     9849700      46.74286
    2015-01-08  47.12000  47.83571 46.47857   47.78000     9601900      47.78000
    2015-01-09  47.63143  48.02000 46.89857   47.04143     9578100      47.04143


.. code:: r

    head(AAPL)



.. parsed-literal::

               AAPL.Open AAPL.High AAPL.Low AAPL.Close AAPL.Volume AAPL.Adjusted
    2015-01-02    111.39    111.44   107.35     109.33    53204600     100.21645
    2015-01-05    108.29    108.65   105.41     106.25    64285500      97.39318
    2015-01-06    106.54    107.43   104.63     106.26    65797100      97.40237
    2015-01-07    107.20    108.20   106.70     107.75    40105900      98.76815
    2015-01-08    109.23    112.15   108.70     111.89    59364500     102.56307
    2015-01-09    112.67    113.25   110.21     112.01    53699500     102.67305


Descargar precios hasta la fecha actual
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

El siguiente ejemplo descarga los precios de las acciones de Netflix y
Apple desde el primero de enero de 2015 hasta el día que se corre el
código, hasta el día de hoy. Esto se hace colocando la fecha del sistema
con ``to = Sys.Date()``.

.. code:: r

    getSymbols(c("NFLX","AAPL"), from="2015-01-01", to = Sys.Date())



.. raw:: html

    <style>
    .list-inline {list-style: none; margin:0; padding: 0}
    .list-inline>li {display: inline-block}
    .list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
    </style>
    <ol class=list-inline><li>'NFLX'</li><li>'AAPL'</li></ol>
    


Los precios descargados se trabajará solo con la columna de los precios
ajustados ``.Adjusted`` que es la columna ``6``. De cada uno de los
objetos que se descargaron y que contienen los precios de las acciones
se deberá extraer sólo la columna de los precios descargados y se
guardaran en objeto que se llamará ``precios``.

La función ``merge`` une los vectores en uno solo objeto que sería una
matriz.

Matriz de precios.
~~~~~~~~~~~~~~~~~~

.. code:: r

    precios = merge( NFLX[,6], AAPL[,6])

.. code:: r

    precios = ts(precios)

.. code:: r

    head(precios)



.. raw:: html

    <table>
    <caption>A matrix: 6 × 2 of type dbl</caption>
    <thead>
    	<tr><th scope=col>NFLX.Adjusted</th><th scope=col>AAPL.Adjusted</th></tr>
    </thead>
    <tbody>
    	<tr><td>49.84857</td><td>100.21645</td></tr>
    	<tr><td>47.31143</td><td> 97.39318</td></tr>
    	<tr><td>46.50143</td><td> 97.40237</td></tr>
    	<tr><td>46.74286</td><td> 98.76815</td></tr>
    	<tr><td>47.78000</td><td>102.56307</td></tr>
    	<tr><td>47.04143</td><td>102.67305</td></tr>
    </tbody>
    </table>
    


.. code:: r

    dim(precios)



.. raw:: html

    <style>
    .list-inline {list-style: none; margin:0; padding: 0}
    .list-inline>li {display: inline-block}
    .list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
    </style>
    <ol class=list-inline><li>1337</li><li>2</li></ol>
    


De cada acción se descargó 1337 filas que corresponden a 1337 precios.

Gráfico del precio
~~~~~~~~~~~~~~~~~~

.. code:: r

    plot(precios, main = "Precios")



.. image:: output_22_0.png
   :width: 420px
   :height: 420px


Matriz de rendimientos
~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    rendimientos = diff(log(precios))

.. code:: r

    head(rendimientos)



.. raw:: html

    <table>
    <caption>A matrix: 6 × 2 of type dbl</caption>
    <thead>
    	<tr><th scope=col>NFLX.Adjusted</th><th scope=col>AAPL.Adjusted</th></tr>
    </thead>
    <tbody>
    	<tr><td>-0.052237977</td><td>-2.857619e-02</td></tr>
    	<tr><td>-0.017268873</td><td> 9.438614e-05</td></tr>
    	<tr><td> 0.005178495</td><td> 1.392460e-02</td></tr>
    	<tr><td> 0.021945626</td><td> 3.770276e-02</td></tr>
    	<tr><td>-0.015578461</td><td> 1.071722e-03</td></tr>
    	<tr><td>-0.032280783</td><td>-2.494930e-02</td></tr>
    </tbody>
    </table>
    


.. code:: r

    dim(rendimientos)



.. raw:: html

    <style>
    .list-inline {list-style: none; margin:0; padding: 0}
    .list-inline>li {display: inline-block}
    .list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
    </style>
    <ol class=list-inline><li>1336</li><li>2</li></ol>
    


.. code:: r

    head(rendimientos)



.. raw:: html

    <table>
    <caption>A matrix: 6 × 2 of type dbl</caption>
    <thead>
    	<tr><th scope=col>NFLX.Adjusted</th><th scope=col>AAPL.Adjusted</th></tr>
    </thead>
    <tbody>
    	<tr><td>-0.052237977</td><td>-2.857619e-02</td></tr>
    	<tr><td>-0.017268873</td><td> 9.438614e-05</td></tr>
    	<tr><td> 0.005178495</td><td> 1.392460e-02</td></tr>
    	<tr><td> 0.021945626</td><td> 3.770276e-02</td></tr>
    	<tr><td>-0.015578461</td><td> 1.071722e-03</td></tr>
    	<tr><td>-0.032280783</td><td>-2.494930e-02</td></tr>
    </tbody>
    </table>
    


Gráfico de rendimientos
~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    plot(rendimientos, main = "Rendimientos")



.. image:: output_29_0.png
   :width: 420px
   :height: 420px


``tseries`` para descargar precios de acciones
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    library(quantmod)

.. code:: r

    library(tseries)

Dentro de esa librería se usa la función ``get.hist.quote``.

A diferencia que la función ``getSymbols``, se debe repetir el código
por cada acción que se quiera descargar y es posible sólo descargar la
columna del precio ajustado con ``quote = "AdjClose"``.

Descargar precios con fechas específicas
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

El siguiente ejemplo extrae los precios de las acciones de Netflix y
Apple desde el primero de enero de 2015 hasta el 14 de abril de 2020.
Note que descarga hasta un día bursátil antes del indicado, que fue el
15 de abril de 2020.

.. code:: r

    NFLX = get.hist.quote(instrument = "NFLX", start = as.Date("2015-01-01"), end= as.Date("2020-04-15"), quote = "AdjClose")


.. parsed-literal::

    time series starts 2015-01-02
    time series ends   2020-04-14
    

.. code:: r

    head(NFLX)



.. parsed-literal::

               Adjusted
    2015-01-02 49.84857
    2015-01-05 47.31143
    2015-01-06 46.50143
    2015-01-07 46.74286
    2015-01-08 47.78000
    2015-01-09 47.04143


.. code:: r

    AAPL = get.hist.quote(instrument = "AAPL", start = as.Date("2015-01-01"), end= as.Date("2020-04-15"), quote = "AdjClose", provider = c("yahoo"))


.. parsed-literal::

    time series starts 2015-01-02
    time series ends   2020-04-14
    

.. code:: r

    head(AAPL)



.. parsed-literal::

                Adjusted
    2015-01-02 100.21645
    2015-01-05  97.39318
    2015-01-06  97.40237
    2015-01-07  98.76815
    2015-01-08 102.56307
    2015-01-09 102.67305


Descargar precios hasta la fecha actual
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

El siguiente ejemplo descarga los precios de las acciones de Netflix y
Apple desde el primero de enero de 2015 hasta el día que se corre el
código, hasta el día de hoy. Esto se hace colocando la fecha del sistema
con ``to = Sys.Date()``.

.. code:: r

    NFLX = get.hist.quote(instrument = "NFLX", start = as.Date("2015-01-01"), end = Sys.Date(), quote = "AdjClose")


.. parsed-literal::

    time series starts 2015-01-02
    time series ends   2020-04-24
    

.. code:: r

    AAPL = get.hist.quote(instrument = "AAPL", start = as.Date("2015-01-01"), end = Sys.Date(), quote = "AdjClose")


.. parsed-literal::

    time series starts 2015-01-02
    time series ends   2020-04-24
    

Matriz de precios.
~~~~~~~~~~~~~~~~~~

.. code:: r

    precios = merge( NFLX, AAPL)

.. code:: r

    precios = ts(precios)

.. code:: r

    head(precios)



.. raw:: html

    <table>
    <caption>A matrix: 6 × 2 of type dbl</caption>
    <thead>
    	<tr><th scope=col>Adjusted.NFLX</th><th scope=col>Adjusted.AAPL</th></tr>
    </thead>
    <tbody>
    	<tr><td>49.84857</td><td>100.21645</td></tr>
    	<tr><td>47.31143</td><td> 97.39318</td></tr>
    	<tr><td>46.50143</td><td> 97.40237</td></tr>
    	<tr><td>46.74286</td><td> 98.76815</td></tr>
    	<tr><td>47.78000</td><td>102.56307</td></tr>
    	<tr><td>47.04143</td><td>102.67305</td></tr>
    </tbody>
    </table>
    


.. code:: r

    dim(precios)



.. raw:: html

    <style>
    .list-inline {list-style: none; margin:0; padding: 0}
    .list-inline>li {display: inline-block}
    .list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
    </style>
    <ol class=list-inline><li>1337</li><li>2</li></ol>
    


De cada acción se descargó 1337 filas que corresponden a 1337 precios.

Gráfico del precio
~~~~~~~~~~~~~~~~~~

.. code:: r

    plot(precios, main = "Precios")



.. image:: output_52_0.png
   :width: 420px
   :height: 420px


Matriz de rendimientos.
~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    rendimientos = diff(log(precios))

.. code:: r

    head(rendimientos)



.. raw:: html

    <table>
    <caption>A matrix: 6 × 2 of type dbl</caption>
    <thead>
    	<tr><th scope=col>Adjusted.NFLX</th><th scope=col>Adjusted.AAPL</th></tr>
    </thead>
    <tbody>
    	<tr><td>-0.052237977</td><td>-2.857619e-02</td></tr>
    	<tr><td>-0.017268873</td><td> 9.438614e-05</td></tr>
    	<tr><td> 0.005178495</td><td> 1.392460e-02</td></tr>
    	<tr><td> 0.021945626</td><td> 3.770276e-02</td></tr>
    	<tr><td>-0.015578461</td><td> 1.071722e-03</td></tr>
    	<tr><td>-0.032280783</td><td>-2.494930e-02</td></tr>
    </tbody>
    </table>
    


Se observa que los nombres de la matriz de rendimientos tienen los
mismos nombres que la matriz de precios.

.. code:: r

    dim(rendimientos)



.. raw:: html

    <style>
    .list-inline {list-style: none; margin:0; padding: 0}
    .list-inline>li {display: inline-block}
    .list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
    </style>
    <ol class=list-inline><li>1336</li><li>2</li></ol>
    


Gráfico de rendimientos
~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

    plot(rendimientos, main = "Rendimientos")



.. image:: output_59_0.png
   :width: 420px
   :height: 420px

