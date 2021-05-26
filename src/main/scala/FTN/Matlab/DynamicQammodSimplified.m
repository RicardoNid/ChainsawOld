function QAMSymbols = DynamicQammodSimplified(bits, bitAlloc)

    segmentHead = 1;
    QAMSymbols = [];

    % bits = bits';

    for i = 1:length(bitAlloc)

        bitAllocated = bitAlloc(i); % ��ǰҪ��������ز�(Ⱥ)������ı�����

        if bitAllocated == 0
            QAMSymbol = 0;
        else
            bitsTobeMapped = bits(segmentHead:segmentHead + bitAllocated - 1, 1); % ���ճ��Ȼ�ȡ��ӳ�����
            segmentHead = bitAllocated + segmentHead; % ά����ȡ����λ��

            QAMSymbol = qammod(bitsTobeMapped, 2^bitAllocated, 'gray', 'InputType', 'bit');
        end

        % carrierPosition = bitAllocSum{i}; % �÷������������Ӧ�����ز�λ��
        QAMSymbols = [QAMSymbols; QAMSymbol];
    end
