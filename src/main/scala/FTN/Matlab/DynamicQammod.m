function QAMSymbols = DynamicQammod(bits)
    global SToPcol
    global OFDMSymbolNumber
    global RmsAlloc
    global SubcarriersNum
    global ConvCodeRate

    %% bit loading %%
    load('./data/bitAlloc.mat'); % 比特分配

    QAMSymbols = zeros(SubcarriersNum, SToPcol);

    % segmentHead = 1;

    % for i = 1:length(bitAlloc)

    %     bitAllocated = bitAlloc(i); % 当前要处理的子载波(群)被分配的比特数

    %     if bitAllocated == 0
    %         QAMSymbol = 0;
    %     else
    %         bitsLength =  OFDMSymbolNumber * bitAllocated / ConvCodeRate;
    %         bitsTobeMapped = bits(segmentHead:segmentHead + bitsLength - 1, 1); % 依照长度获取待映射比特
    %         segmentHead = bitsLength + segmentHead; % 维护待取比特位置

    %         QAMSymbol = Qammod(bitAllocated, bitsTobeMapped); % 依照分配比特数映射
    %         QAMSymbol = QAMSymbol / RmsAlloc(bitAllocated);
    %         QAMSymbol = reshape(QAMSymbol, 1, SToPcol); % 串->并转换
    %     end

    %     QAMSymbols(i, :) = QAMSymbol; % 拼装
    % end

    bitGroups = reshape(bits, 14336 / 16, 16);
    for i = 1 : 16
        bitsOneCycle = bitGroups(:, i)';
        symbolsOneCycle = SingleDynamicQammod(bitsOneCycle, bitAlloc);
        QAMSymbols(:, i) = symbolsOneCycle;
    end
    