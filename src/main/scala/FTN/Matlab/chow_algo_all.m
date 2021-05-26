function [bits_allo, power_allo, total_bits] = chow_algo_all(SNR, gap)
    global SubcarriersNum
    global TargetBits
    %--------------------������� -------------------------
    % SNR          ÿ�����ŵ�������ȣ�1��N_subc)���� (dB)
    % SubcarriersNum       ���ز���
    % gap          ����ȼ�϶��������(dB)
    % TargetBits  �ܱ����������ݴ������ʣ�
    %--------------------�������------------------------
    % bits_allo    ���ط���
    % power_allo   ���ʷ���
    % Iterate_count ��������
    % --------------------��ʼ��-------------------------
    margin = 1; %����ֵ(dB)
    Max_count = 10; %����������
    Iterate_count = 0; %����������
    total_bits = 0; %������ܱ�����
    round_bits = zeros(1, SubcarriersNum); %ÿ�����ز�����ı�����ȡ��ֵ
    difference = zeros(1, SubcarriersNum); %ÿ�����ز����ط������������
    %-----------------------------���ط���-------------------------
    while (total_bits ~= TargetBits) && (Iterate_count < Max_count)
        %--------------------------------------------------------------
        Iterate_count = Iterate_count + 1;
        temp_bits = log2(1 + SNR ./ (gap * margin));
        round_bits = round(temp_bits);
        difference = temp_bits - round_bits;
        %--------------------------------------------------------------
        total_bits = sum(round_bits);

        if (total_bits == 0)
            disp('�ŵ�����ʹ��');
            break;
        end

        nuc = length(find(round_bits == 0)); %
        N_use = SubcarriersNum - nuc; %
        %     ========================�㷨�޸�========================
        margin = margin * 2^((total_bits - TargetBits) / N_use);
    end

    % %------------------------------�����޸�--------------------------
    while (total_bits > TargetBits)
        use_ind = find(round_bits > 0);
        diff_use = difference(use_ind);
        id = find(diff_use == min(diff_use), 1); %�ú������������ţ��Ķ�Ӧ��ϵ
        ind_alter = use_ind(id); %�ú������������ţ��Ķ�Ӧ��ϵ
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
    %--------------------------���ʷ���-----------------------------

    % 6.��gap��������,
    power_allo = ((2.^bits_allo - 1) * (gap * margin)) ./ SNR;

end
