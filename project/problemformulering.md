# Project Description

Projekt af caro og mikkel  

Vi vil gerne implementere grovers algoritme i sml.  
Vi vil gerne implementere byggeblokkene i sml, som kan blive compilet til Futhark.  

Læs:  
grovers paper  
https://learn.microsoft.com/en-us/azure/quantum/tutorial-qdk-grovers-search?tabs=tabid-copilot
https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=10014648

Fremlæggelse:
30 min i alt

10 min: Literature review
	- grovers algoritme
		- hvad er det?
			- unstructured search
		- hvorfor er det smart?
			- O(sqrt(N)) mod konventionel O(N)
		- hvordan bliver det beskrevet i grovers paper
		- den bliver henvist til i mange papers
	- Mere konkret implementation
		- finde nogen, som har implementeret det
10 min: Project description
	- sml framework
	- vi vil gerne simulere det i sml
	- vi har lavet nogle af byggeklodserne ved at lave øvelserne
	- Så vi vil lave de resterende byggeklodser, som der er brug for, for at implementere grovers algoritme
	- sætte det hele sammen til algoritmen
	- Hvis vi har mere tid
		- compile det til futhark
		- køre det på en kvantecomputer
		- implementere en anden kvante algoritme
		- spise popcorn
10 min: QA
	- stil også nogle fucking spørgsmål
