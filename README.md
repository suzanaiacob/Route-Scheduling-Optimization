# Route Scheduling Optimization

This is an optimization project using location data to schedule transportation routes.

This project was completed aliongside my MIT colleagues using real data from a logistics company. The goal was to schedule optimal transportation routes which reach a series of clients and arrive at the final destination according to pre-defined constraints (e.g. specific arrival and departure times, multiple arrivals at specific clients). The objective was to optimize for early arrivals at the final destination. 

The project has three parts:
- Data cleaning and exploratory data analysis (developed in R and Tableau)
- Obtaining location and distance data using the Google Maps API (written in JavaScript): https://developers.google.com/maps/documentation/javascript/distancematrix 
- Developing the optimization algorithm (written in Julia using Jump and Gurobi)

The data used is confidential client data (primarily client addressed), but I am sharing the code which generally could be applied to a route scheduling with any addresses.  

