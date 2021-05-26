function Demodulated = DynamicQamdemod(FDE)

    load('./data/bitAlloc.mat');
    % load('./data/bitAllocSort.mat');
    % load('./data/bitAllocSum.mat');

    % ?? ����Ĵ����޷�������ط�����0�����
    Demodulated = [];

    % for i = 1:length(bitAllocSort)

    %     bitAllocated = bitAllocSort(i); % ��ǰҪ��������ز�(Ⱥ)������ı�����

    %     if bitAllocated ~= 0
    %         carrierPosition = bitAllocSum{i}; % ����ı���������Ӧ�����ز�
    %         QAM = reshape(FDE(carrierPosition, :), [], 1); % ��ȡ����ӳ�����,��->��ת��
    %         demodulated = Qamdemod(bitAllocated, QAM); % ���շ����������ӳ��
    %         Demodulated = [Demodulated, demodulated]; % ����ƴ�ӷ�ʽ��ӳ��ʱһ��,�������ݱ�������ͬ������
    %     end

    % end

    for i = 1:length(bitAlloc)

        bitAllocated = bitAlloc(i); % ��ǰҪ��������ز�(Ⱥ)������ı�����

        if bitAllocated ~= 0
            QAM = reshape(FDE(i, :), [], 1); % ��ȡ����ӳ�����,��->��ת��
            demodulated = Qamdemod(bitAllocated, QAM); % ���շ����������ӳ��
            Demodulated = [Demodulated, demodulated]; % ����ƴ�ӷ�ʽ��ӳ��ʱһ��,�������ݱ�������ͬ������
        end

    end
