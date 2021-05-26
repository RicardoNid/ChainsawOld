function QAMSymbols = DynamicQammod(bits)
    global SToPcol
    global OFDMSymbolNumber
    global RmsAlloc
    global SubcarriersNum
    global ConvCodeRate

    %% bit loading %%
    load('./data/bitAlloc.mat') % ���ط���
    load('./data/bitAllocSort.mat');
    load('./data/bitAllocSum.mat');

    QAMSymbols = zeros(SubcarriersNum, SToPcol);

    segmentHead = 1;

    for i = 1:length(bitAllocSort)

        bitAllocated = bitAllocSort(i); % ��ǰҪ��������ز�(Ⱥ)������ı�����

        if bitAllocated == 0
            QAMSymbol = 0;
        else
            bitsLength = OFDMSymbolNumber * bitAllocated * length(bitAllocSum{i}) / ConvCodeRate; % �����ܳ���
            bitsTobeMapped = bits(segmentHead:segmentHead + bitsLength - 1, 1); % ���ճ��Ȼ�ȡ��ӳ�����
            segmentHead = bitsLength + segmentHead; % ά����ȡ����λ��

            QAMSymbol = Qammod(bitAllocated, bitsTobeMapped); % ���շ��������ӳ��
            % QAMSymbol = QAMSymbol / RmsAlloc(bitAllocated); % ?? ��̬��һ��
            QAMSymbol = QAMSymbol / rms(QAMSymbol); % ?? ��̬��һ��
            QAMSymbol = reshape(QAMSymbol, length(bitAllocSum{i}), SToPcol); % ��->��ת��
        end

        carrierPosition = bitAllocSum{i}; % �÷������������Ӧ�����ز�λ��
        QAMSymbols(carrierPosition, :) = QAMSymbol; % ��ӳ���ķ��Ŵ�->��װ�������Ӧ���ز�λ��
    end
