# Operas in the Burgtheater, 14 Apr 1789 to 7 Mar 1791

This is a Shiny app for examining the receipts from all opera performanes in the old Burgtheater 
in Vienna in the seasons 1789/90 (14 Apr 1789 to 11 Feb 1790) and 1790/91 (13 Apr 1790 to 7 Mar 1791).  

The operas performed in those two seasons included the first revival of Mozart's *Le nozze di Figaro*, 
and the premiere run of his *Così fan tutte*. The app offers a tabular view of the data (using the DT package), 
a table of summary statistics (count, median, mean, standard deviation), comparative boxplots and histograms 
(both using ggplot2), and a time series (using the dygraphs package).

There are six columns in the dataset:

* **Date**: the date of the performance
* **DOW**: the day of the week
* **Title**: the title of the opera
* **Composer**: the composer of the opera
* **Receipts**: the receipts from the performance in **kreuzer** (kr)
* **Zinz**: A boolean field (True or False) indicating whether Count Karl Zinzendorf attended the performance.

The data is taken from a ledger recording the box-office receipts from all performances by the company of the 
court theater in Vienna during those two seasons (Vienna, Österreichisches Theatermuseum, M 4000), supplemented 
by the diaries of Count Zinzendorf (Vienna, Haus-, Hof- und Staatsarchiv). 

The dataset used in this app is a subset of a larger one that includes all performances by the court theater 
during this same period, including operas, plays, and melodramas, with additional fields (including Author and Month). 

I have written in detail about the implications of the box-office data for the reception of Mozart's operas in: 

>Edge, Dexter. 1996. "Mozart’s Reception in Vienna, 1787–1791." In: <em>Wolfgang Amadè Mozart: 
Essays on his Life and his Music</em>, edited by Stanley Sadie. 66–117. Oxford: Clarendon Press. 
[<a href="https://www.academia.edu/7004284/Mozarts_Reception_in_Vienna_1787-1791">academia.edu</a>]

A new analysis of this data using R and the tidyverse packages is in progress.

For comments, feature requests, and bug reports, contact me at:
<dexedge@gmail.com>
