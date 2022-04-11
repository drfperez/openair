# openair
R code to analyse air pollution hourly data from Gencat and Meteocat servers.
You only need to do minor edition at Visual Code Studio to adapt code to your city databases and to call from RStudio 
e.g. source ("C://...datamanipulation.R")
Air pollutant hourly data are from Martorell starting 20/11/1991 until 30/03/2022 available in the file city.csv obtained from XVPCA Gencat server.
Half-hourly data is combined with wind direction (variable code 30) and wind speed (variable code 31) from Castellbisbal (station code XC) from 20/05/2009 available at XEMA Meteocat server: https://analisi.transparenciacatalunya.cat/Medi-Ambient/Dades-meteorol-giques-de-la-XEMA/nzvn-apee/data
Find the list of XEMA meteo stations at https://www.meteo.cat/observacions/llistat-xema containing half-hourly wind data
Find the air pollutants hourly data at https://mediambient.gencat.cat/ca/05_ambits_dactuacio/atmosfera/qualitat_de_laire/vols-saber-que-respires/descarrega-de-dades/descarrega-dades-automatiques/
