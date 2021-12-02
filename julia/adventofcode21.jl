# Advent of Code

# Day 1
#Part A
deeper = function (depth)
    d = 0
    for i in 2:length(depth) 
       if depth[i] > depth[i-1]
         d += 1  
       end 
    end
    d
end

# Part B
deeper_window = function (depth)
    d = 0
    for i in 4:length(depth) 
        cur_sum = sum(depth[i-2:i])
        prev_sum = sum(depth[i-3:i-1])
       if cur_sum > prev_sum
         d += 1  
       end 
    end
    d
end

#Get data
d1 = parse.(Int, readlines("inst/input01.txt"))

deeper(d1)
deeper_window(d1)