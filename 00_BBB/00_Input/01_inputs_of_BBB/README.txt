In this folder, there are six txt files. They are the input files to generate the BBB results.

1. counts_fm.txt [this is the family level analysis input file] and modern_fm.txt [modern species diversity of families]
2. counts_ord.txt [this is the order level analysis input file] and modern_ord.txt [modern family diversity of orders]
3. counts_whole_angio.txt [this is the whole angiosperm analysis input file; inside the file, is ten random repeats of the same clade] and modern_whole_angio.txt [modern order diversity of the whole angiosperms]

The first column in three 'counts' txt files is time bins (each unit is Ma, size is 2.5). The first row are the names of the clades.

The unit for numbers in each group (applied to all six files):
	for families, use species as a unit;
	for orders, use family as a unit;
	for crown angiosperm, use order as a unit.




Other notes:

'whole angiosperms' runs 10 independent tests and combine all results in an output. So here we have one counts file and one modern file, but both contains 10 tests.

orders inputs, the previous counts file named as in R script should be 'angiosperms_ord-0214.txt', just changed here for formatting better.

families inputs comes directly from Silvestro et al., 2021 (https://doi.org/10.1038/s41559-020-01387-8). But they previously used first version of BBB which not allow extinct clade as estimation subject. Here I used the extended version which published officially in Carlisle et al., 2023 (https://doi.org/10.1016/j.cub.2023.06.016).

for the 'modern diversity':
	family - modern species number in each family data get from Silvestro et al., 2021
	order - modern families number in each order is calculated from Christenhusz and Byng, 2016 (https://doi.org/10.11646/phytotaxa.261.3.1)
	whole angiosperms - modern orders number 64 is calculated from Qian et al., 2022 (doi: 10.17520/biods.2022254)

