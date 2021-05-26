function OFDMSymbols = FFT(OFDMSignals)
    global IsPreamble
    global OFDMPositions
    global PreambleNumber
    global PreambleCarrierPositions
    global DataCarrierPositions
    global FFTSize
    global CPLength
    global SToPcol

    if IsPreamble == 1;
        symbolLength = PreambleNumber;
        positionsOut = PreambleCarrierPositions;
        positionsIn = 1:FFTSize;
    else
        symbolLength = SToPcol;
        positionsOut = DataCarrierPositions;
        positionsIn = OFDMPositions;
    end

    %% 处理变换前的数据
    OFDMSignals = reshape(OFDMSignals, [], symbolLength); % 串->并转换
    OFDMSignals = OFDMSignals(CPLength / 2 + 1:end - CPLength / 2, :); % 去掉循环前缀
    %% 填充变换前的数据
    fftBlock = zeros(FFTSize, symbolLength);
    fftBlock(1:length(positionsIn), :) = OFDMSignals;
    %% 进行变换
    OFDMSignals = fft(fftBlock); % 标准fft
    %% 提取变换后的数据
    OFDMSymbols = OFDMSignals(positionsOut, :); % 提取需要的子载波
