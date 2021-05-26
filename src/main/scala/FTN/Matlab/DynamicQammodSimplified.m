function QAMSymbols = DynamicQammodSimplified(bits, bitAlloc)

    segmentHead = 1;
    QAMSymbols = [];

    % bits = bits';

    for i = 1:length(bitAlloc)

        bitAllocated = bitAlloc(i); % 当前要处理的子载波(群)被分配的比特数

        if bitAllocated == 0
            QAMSymbol = 0;
        else
            bitsTobeMapped = bits(segmentHead:segmentHead + bitAllocated - 1, 1); % 依照长度获取待映射比特
            segmentHead = bitAllocated + segmentHead; % 维护待取比特位置

            QAMSymbol = qammod(bitsTobeMapped, 2^bitAllocated, 'gray', 'InputType', 'bit');
        end

        % carrierPosition = bitAllocSum{i}; % 该分配比特数所对应的子载波位置
        QAMSymbols = [QAMSymbols; QAMSymbol];
    end
