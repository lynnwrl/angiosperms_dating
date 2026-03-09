Two alternative strategies were tested for handling unnamed higher-level taxa (e.g., unnamed families or orders) when generating the fossil occurrence matrices.

Option 1: Treat all unnamed taxa as the same group (e.g., unnamed_fm, unnamed_ord).
Option 2: Treat each unnamed taxon as a distinct group (e.g., unnamed_ord_1, unnamed_ord_2, etc.).

Both strategies produced very similar age estimates in the BBB analyses. Because Option 1 yields slightly more conservative age estimates, it was selected for the final analysis. The final result corresponds to the log file:

00_whole_angiosperms/all_angio_one_order.log

The script used for the final analysis is:

sort_data_whole.R

The folder files/ contains example outputs from this script.
Folders option1_10replicates/ and option2_10replicates/ contain the replicate datasets generated for testing the two strategies.