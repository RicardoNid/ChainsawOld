function [OFDMSymbols] = IFFT(QAMSymbols)
    global IsPreamble
    global PreambleCarrierPositions
    global DataCarrierPositions
    global FFTSize
    global OFDMPositions
    global CPLength
    global SToPcol

    if IsPreamble == 1;
        symbolLength = 1;
        positionsIn = PreambleCarrierPositions;
        positionsOut = 1:FFTSize;
    else
        symbolLength = SToPcol;
        positionsIn = DataCarrierPositions;
        positionsOut = OFDMPositions;
    end

    %% 填充变换前的数据
    ifftBlock = zeros(FFTSize, symbolLength); % 预先padding
    ifftBlock(positionsIn, :) = QAMSymbols; % "放置"QAM符号
    ifftBlock(FFTSize + 2 - positionsIn, :) = conj(ifftBlock(positionsIn, :)); % "放置"其共轭
    %% 进行变换
    OFDMSymbols = ifft(ifftBlock); % 标准ifft
    %% 处理变换后的数据
    OFDMSymbols = OFDMSymbols(1:length(positionsOut), :); % 提取需要的子载波
    OFDMSymbols = [OFDMSymbols(end - CPLength / 2 + 1:end, :); OFDMSymbols; OFDMSymbols(1:CPLength / 2, :)]; % 增加循环前缀
    OFDMSymbols = reshape(OFDMSymbols, [], 1); % 并->串转换
