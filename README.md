# Public transport of Alcobendas - Shiny App

This app contains information about transport of Alcobendas, this information was collect by Ayuntamiento de Alcobendas. In this app we will different statistical plots that let you to infer how was the behavior of the public transport in Alcobendas between 2015 and 2017. 

## Data description 

The data contains 84 variables about 28 bus lines for 3 years in Alcobendas. The variables that are described in this dataset are: 

- Line: Is the number of the linebus  
- Year: Corresponds to the year in which it was collected
- Tipo de transporte: Refers to the type of the line, it can be urban bus and interurban bus
- Número anual de pasajeros: Refers to the annual number of passengers in a certain line 
- Expediciones por día: Refers to the number of times that the bus was in services
- Viajeros por día:Refers to the daily number of passengers in a certain line
- Viajeros por expedición: Refers to the number passengers in each time in which the bus was in services
- Kilometros anuales recorridos: Refers to the number of kilometers that the bus toured in a year. 

In the next section you can find the functionalities of the app: 

### Data

In the first panel you can see the data from the bus line and the year that you selected previously. Also, in this panel you can generate a  PDF report that will show you a histogram plotted with the information that you selected. 

### Histogram 

In this panel you have the option to manipulate a histogram selecting the variables that you want to plot and magnitude of the bins. This can of manipulation let you to understand the data distribution of each variable.

### Correlation graph 

The correlation graph is used to observe the relation between to variables. The new option that you can found in this plot is that if you click in a point the information of this line will appear in the box called "Points near click". On the other hand, if you select a certain quantity of points the information of the lines will appear in the box called "Brushed points"

### Other plots 

Finally, in the last panel you can select the variable that you want to plot and the type of plot that you want between a density plot and a barchart. For both plots you can diferentiate between the type of bus, it means with a blue color the urban autobus and with red color the interurban bus. 


