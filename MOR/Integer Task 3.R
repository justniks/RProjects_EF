## Формализация задачи ##
# x_i -- кол-во работников, которые работают, начиная с i-й смены
# x_1 -- кол-во работников, которые пришли к 00:00
# x_2 -- кол-во работников, которые пришли к 04:00
#
# с 04:00 до 08:00 работают и те, кто пришли к 00:00, и те, кто пришел к 04:00
#
#
# z = \sum_i x_i \to min
#
# x_1 + x_2 >= 7
# x_3 + x_4 >= 12
# x_4 + x_5 >= 15
# x_5 + x_6 >= 10
# x_6 + x_1 >= 5
#
# x_j \in \mathbb{Z}_{+}


#### Осталось решить ...