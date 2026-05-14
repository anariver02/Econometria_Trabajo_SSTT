# Econometria_Trabajo_SSTT: Análisis temporal del transporte urbano por autobús en España

Este repositorio contiene los materiales utilizados para el trabajo de Econometría sobre el análisis y previsión de una serie temporal española con estacionalidad mensual.

La serie analizada es el número de viajeros de transporte urbano por autobús en España, expresado en miles de viajeros. La muestra utilizada cubre el periodo 2012M01-2023M12. El año 2023 se reserva como muestra de validación para comparar la capacidad predictiva de varios modelos SARIMA, y posteriormente se obtiene una previsión fuera de muestra para 2024.

## Objetivo del trabajo

El objetivo es aplicar la metodología Box-Jenkins a una serie temporal mensual con estacionalidad, siguiendo las etapas de:

1. Análisis gráfico inicial.
2. Transformación logarítmica.
3. Diferenciación regular y estacional.
4. Identificación mediante FAS y FAP.
5. Estimación de modelos SARIMA por máxima verosimilitud.
6. Diagnosis de residuos.
7. Comparación de previsiones en el año reservado.
8. Selección del modelo final y previsión para 2024.

## Datos

La fuente de datos es el Instituto Nacional de Estadística (INE).

- Serie: Transporte urbano por autobús. Nivel nacional.
- Frecuencia: mensual.
- Unidad: miles de viajeros.
- Periodo utilizado: 2012M01-2023M12.

El archivo de datos utilizado en el análisis es:

```text
transporte AUTOBUS SERIE COMPLETA.xls
