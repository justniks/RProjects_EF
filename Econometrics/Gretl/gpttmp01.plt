# gpstyle dark2
set linetype 1 pt 6  lc rgb "#1B9E77" # dark teal
set linetype 2 pt 7  lc rgb "#D95F02" # dark orange
set linetype 3 pt 11 lc rgb "#7570B3" # dark lilac
set linetype 4 pt 13 lc rgb "#E7298A" # dark magenta
set linetype 5 pt 4  lc rgb "#66A61E" # dark lime green
set linetype 6 pt 3  lc rgb "#E6AB02" # dark banana
set linetype 7 pt 5  lc rgb "#A6761D" # dark tan
set linetype 8 pt 2  lc rgb "#666666" # dark gray
set border 3 back lc rgb "#808080"
set tics nomirror
# X = 'TotalArea' (3)
# Y = 'Price' (1)
set xlabel 'TotalArea'
set xzeroaxis
set datafile missing "?"
# plot includes automatic fit: OLS
set title "Price от TotalArea (с подбором по МНК)"
set ylabel 'Price'
set key left top
set xrange [23.8625:46.4375]
plot \
 '-' using 1:2 title "" w points, \
786456.18 + 135317.06*x title "Y = 7.86e+05 + 1.35e+05X" w lines
43.9 8636886 
39 8337771 
38.3 7984171 
35.2 7537622 
37.9 7381177 
38.6 7231000 
39.8 7063700 
41.3 6973600 
41.6 6935000 
37.8 6749600 
37.6 6458477 
37.6 6450400 
35.5 6427605 
45.9 6373000 
37.9 6323000 
36 6236000 
34.8 6189673 
35.8 6138000 
38.5 6101596 
42 6057912 
37.7 6047000 
34.1 6044430 
38.9 6044243 
35.3 6041000 
39.1 6022000 
33.8 5957000 
38.8 5946000 
37.7 5931600 
37.8 5896762 
38.5 5896545 
39 5877027 
38 5862000 
34.4 5853000 
34 5839194 
37.9 5824527 
33.5 5805000 
38.1 5786000 
37.9 5772000 
36.6 5755000 
37.8 5752328 
35.6 5720813 
36.9 5674000 
33 5667820 
39.4 5654500 
36.7 5652497 
36.6 5649503 
35 5622505 
32.6 5608048 
39 5594589 
32.8 5585578 
38.3 5526996 
37.6 5506000 
37.9 5485480 
33.4 5474000 
37.2 5443000 
35.2 5433120 
31.8 5424312 
32.6 5406000 
32.4 5401000 
36.5 5399000 
33.2 5389700 
32.9 5373000 
34.7 5370727 
32.9 5358000 
39.4 5324949 
32.8 5321000 
38.9 5317000 
31.1 5313839 
34.7 5298000 
38.7 5296000 
32.7 5261267 
29.2 5255000 
36.8 5251000 
30.6 5237000 
31.2 5233769 
29.9 5225085 
28.5 5223069 
33.3 5203158 
31.6 5202090 
38.9 5201000 
29.8 5200785 
34.3 5198800 
36 5163228 
30.4 5159000 
32.5 5150000 
34.5 5147918 
30.3 5136200 
30.2 5136000 
34.1 5125401 
32.7 5115800 
35.1 5101430 
33.2 5062104 
30 5060850 
35.1 5055488 
35.8 5018193 
37.6 5015973 
32.6 5011000 
38.2 5008440 
32.8 4994000 
28.8 4948790 
32.5 4925000 
40 4916480 
34.2 4828900 
38.6 4814771 
39 4808271 
32.6 4789000 
31.4 4758482 
32.8 4702139 
34.6 4668000 
30.8 4666600 
33.3 4587000 
32.5 4582000 
32.9 4573000 
31.6 4561000 
31.7 4483331 
29.8 4405304 
24.4 4318141 
31.8 4318027 
32.5 4285073 
33 4240300 
30.5 4218000 
e
