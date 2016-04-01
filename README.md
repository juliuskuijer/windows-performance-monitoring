# Windows performance monitoring

This script can be used for analysing Windows perfomance monitor files. The orginal log files (.blg) were converted to .csv with the relog tool. (relog.exe "server.blg" -f csv -o "server.csv" -y)

The R script is cleaning the data and creating 5 outputs: 

- Graphical view of all counters (8 a page)
- Graphical view of filtered counters (8 a page)
- Graphical view of key counters (8 a page)
- Graphical overview of filtered counters (line, boxplot and summary table) (2 a page)
- Table view of summary data by counter

