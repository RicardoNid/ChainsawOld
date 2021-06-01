function QAMSymbols = DynamicQammod(bits)
    global SToPcol
    global OFDMSymbolNumber
    global RmsAlloc
    global SubcarriersNum
    global ConvCodeRate

    %% bit loading %%
    load('./data/bitAlloc.mat'); % ���ط���

    QAMSymbols = zeros(SubcarriersNum, SToPcol);

    % segmentHead = 1;

    % for i = 1:length(bitAlloc)

    %     bitAllocated = bitAlloc(i); % ��ǰҪ��������ز�(Ⱥ)������ı�����

    %     if bitAllocated == 0
    %         QAMSymbol = 0;
    %     else
    %         bitsLength =  OFDMSymbolNumber * bitAllocated / ConvCodeRate;
    %         bitsTobeMapped = bits(segmentHead:segmentHead + bitsLength - 1, 1); % ���ճ��Ȼ�ȡ��ӳ�����
    %         segmentHead = bitsLength + segmentHead; % ά����ȡ����λ��

    %         QAMSymbol = Qammod(bitAllocated, bitsTobeMapped); % ���շ��������ӳ��
    %         QAMSymbol = QAMSymbol / RmsAlloc(bitAllocated);
    %         QAMSymbol = reshape(QAMSymbol, 1, SToPcol); % ��->��ת��
    %     end

    %     QAMSymbols(i, :) = QAMSymbol; % ƴװ
    % end

    bitGroups = reshape(bits, 14336 / 16, 16);
    for i = 1 : 16
        bitsOneCycle = bitGroups(:, i)';
        symbolsOneCycle = SingleDynamicQammod(bitsOneCycle, bitAlloc);
        QAMSymbols(:, i) = symbolsOneCycle;
    end
    