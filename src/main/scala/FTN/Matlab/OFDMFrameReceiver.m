function [decoded] = OFDMFrameReceiver(recvOFDMFrame)
    global On
    global IsPreamble
    global PowerOn
    global CPLength
    global DataCarrierPositions
    global PreambleNumber
    global SToPcol
    global FFTSize
    global RmsAlloc

    %% 下面部分对应于图1
    recvPreambleOFDMSymbols = recvOFDMFrame(1:PreambleNumber * (FFTSize + CPLength)); % 将收到的子帧分为训练序列和信息序列
    recvMsgOFDMSymbols = recvOFDMFrame(PreambleNumber * (FFTSize + CPLength) + 1:end);

    IsPreamble = 1;
    reambleQAMSymbols = FFT(recvPreambleOFDMSymbols);

    IsPreamble = 0;
    FDE = FFT(recvMsgOFDMSymbols); % fft,FDE尺寸224*16

    % 信道估计和均?
    H = ChannelEstimation(reambleQAMSymbols); % 信道估计,得到(训练序列所占据的)各个子载波上的修正系数,H尺寸255*1

    % 信道均?
    for i = 1:SToPcol;
        FDE(:, i) = FDE(:, i) ./ H(DataCarrierPositions - 1); % ?? 此处的子载波对齐可能有误
    end

    %% 下面部分对应于图2
    diffQAMSymbols = Iterating(FDE);

    %% 下面部分对应于图3
    PowerOn = 0; % 去功率分配
    diffQAMSymbols = PowerOnOff(diffQAMSymbols);

    decoded = QAM2Bits(diffQAMSymbols); % QAM解映射 -> 解交织 -> 维特比译码

    if On == 0
        % ?? 此处可能也是不必要的
        dataQAMSymbolsForAlloc = diffQAMSymbols * RmsAlloc(4);
        Alloc(dataQAMSymbolsForAlloc);
    end
