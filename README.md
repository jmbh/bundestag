# bundestag
Data and code for the visualization of the voting pattern of the German parliament


preprocessing.R takes the data downloaded from bundestag.de and preprocesses them in the the following three data files:

- Data_persons.RDS, a 139 (bills) x 659 (members of parliament) table
- Data_vote.X.person.RDS and, a 659 (members of parliament) x 3 (given name, surname, party) table
- Data_votes.RDS, a a 139 (bills) x 4 (data file, title of bill, date voted, days from day 1) table

analysis.R uses these three data files to produce all figures shown in the post here: http://jmbh.github.io/Analyzing-voting-pattern-of-German-parliament/

bundestag_aux_functions.R is sourced by analysis.R and contains functions used in analysis.R.