function [bits_allo, power_allo, total_bits] = chow_algo_all(SNR, gap)
    global SubcarriersNum
    global TargetBits
    %--------------------输入变量 -------------------------
    % SNR          每个子信道的信噪比（1×N_subc)向量 (dB)
    % SubcarriersNum       子载波数
    % gap          信噪比间隙（常量）(dB)
    % TargetBits  总比特数（数据传输速率）
    %--------------------输出变量------------------------
    % bits_allo    比特分配
    % power_allo   功率分配
    % Iterate_count 迭代次数
    % --------------------初始化-------------------------
    margin = 1; %门限值(dB)
    Max_count = 10; %最大迭代次数
    Iterate_count = 0; %迭代计数器
    total_bits = 0; %分配的总比特数
    round_bits = zeros(1, SubcarriersNum); %每个子载波分配的比特数取整值
    difference = zeros(1, SubcarriersNum); %每个子载波比特分配的误差（余量）
    %-----------------------------比特分配-------------------------
    while (total_bits ~= TargetBits) && (Iterate_count < Max_count)
        %--------------------------------------------------------------
        Iterate_count = Iterate_count + 1;
        temp_bits = log2(1 + SNR ./ (gap * margin));
        round_bits = round(temp_bits);
        difference = temp_bits - round_bits;
        %--------------------------------------------------------------
        total_bits = sum(round_bits);

        if (total_bits == 0)
            disp('信道不被使用');
            break;
        end

        nuc = length(find(round_bits == 0)); %
        N_use = SubcarriersNum - nuc; %
        %     ========================算法修改========================
        margin = margin * 2^((total_bits - TargetBits) / N_use);
    end

    % %------------------------------比特修改--------------------------
    while (total_bits > TargetBits)
        use_ind = find(round_bits > 0);
        diff_use = difference(use_ind);
        id = find(diff_use == min(diff_use), 1); %好好理解索引（序号）的对应关系
        ind_alter = use_ind(id); %好好理解索引（序号）的对应关系
        round_bits(ind_alter) = round_bits(ind_alter) - 1;
        difference(ind_alter) = difference(ind_alter) + 1;
        total_bits = sum(round_bits);
    end

    while (total_bits ~= 0 && total_bits < TargetBits)
        use_ind = find(round_bits ~= 0);
        diff_use = difference(use_ind);
        id = find(diff_use == max(diff_use), 1);
        ind_alter = use_ind(id);
        round_bits(ind_alter) = round_bits(ind_alter) + 1;
        difference(ind_alter) = difference(ind_alter) - 1;
        total_bits = sum(round_bits);
    end

    bits_allo = round_bits;
    %--------------------------功率分配-----------------------------

    % 6.把gap放在上面,
    power_allo = ((2.^bits_allo - 1) * (gap * margin)) ./ SNR;

end
