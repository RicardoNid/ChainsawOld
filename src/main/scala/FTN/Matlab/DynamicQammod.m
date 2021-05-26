function QAMSymbols = DynamicQammod(bits)
    global SToPcol
    global OFDMSymbolNumber
    global RmsAlloc
    global SubcarriersNum
    global ConvCodeRate

    %% bit loading %%
    load('./data/bitAlloc.mat') % 比特分配
    load('./data/bitAllocSort.mat');
    load('./data/bitAllocSum.mat');

    QAMSymbols = zeros(SubcarriersNum, SToPcol);

    segmentHead = 1;

    for i = 1:length(bitAllocSort)

        bitAllocated = bitAllocSort(i); % 当前要处理的子载波(群)被分配的比特数

        if bitAllocated == 0
            QAMSymbol = 0;
        else
            bitsLength = OFDMSymbolNumber * bitAllocated * length(bitAllocSum{i}) / ConvCodeRate; % 计算总长度
            bitsTobeMapped = bits(segmentHead:segmentHead + bitsLength - 1, 1); % 依照长度获取待映射比特
            segmentHead = bitsLength + segmentHead; % 维护待取比特位置

            QAMSymbol = Qammod(bitAllocated, bitsTobeMapped); % 依照分配比特数映射
            % QAMSymbol = QAMSymbol / RmsAlloc(bitAllocated); % ?? 静态归一化
            QAMSymbol = QAMSymbol / rms(QAMSymbol); % ?? 静态归一化
            QAMSymbol = reshape(QAMSymbol, length(bitAllocSum{i}), SToPcol); % 串->并转换
        end

        carrierPosition = bitAllocSum{i}; % 该分配比特数所对应的子载波位置
        QAMSymbols(carrierPosition, :) = QAMSymbol; % 将映射后的符号串->并装换填入对应子载波位置
    end
