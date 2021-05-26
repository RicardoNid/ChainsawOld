%  // ======================================================================
%  //  Jinan University
%  //  @Author: JiZhou CanyangXiong
%  //  @Last Modified time: 2021-03-05
%  //  @description: 将每种比特调制对应的载波索引进行集合分类
%  // ======================================================================
function [bits_alloc_sort, bits_alloc_sub_sum] = bits_alloc_position_sum(bits_alloc)
    global SubcarriersNum
    bits_alloc_sort = unique(bits_alloc);

    row = (1:SubcarriersNum);
    bits_alloc_1 = [bits_alloc; row];
    bits_alloc_1_num = length(bits_alloc_sort);
    bits_alloc_sub_sum = cell(1, bits_alloc_1_num);

    for i = 1:bits_alloc_1_num
        [~, y] = find(bits_alloc_1(1, :) == bits_alloc_sort(i));
        bits_alloc_sub_sum{i} = bits_alloc_1(2, y);
    end

end
