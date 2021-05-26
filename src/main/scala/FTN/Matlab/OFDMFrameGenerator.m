function OFDMSymbols = OFDMFrameGenerator(msgBits)

    global On
    global IsPreamble
    global PowerOn
    global PreambleNumber

    %% 数据通路
    % 读取预先存储的训练序列QAM符号
    load './data/preambleQAMSymbols' preambleQAMSymbols

    if On == 0 % 训练模式时,读取预先存储的训练子帧QAM符号
        % msgQAMSymbols = Bits2QAM(msgBits);
        load './data/msgQAMSymbols'
    else
        % 工作模式时,将信息比特加工为QAM符号
        msgQAMSymbols = Bits2QAM(msgBits); % 卷积编码 -> 交织 -> QAM映射
    end

    PowerOn = 1; % 加功率分配
    msgQAMSymbols = PowerOnOff(msgQAMSymbols);

    %% 将训练QAM符号加工为OFDM符号
    IsPreamble = 1;
    preambleOFDMSymbols = IFFT(preambleQAMSymbols);

    %% 将信息QAM符号加工为OFDM符号
    IsPreamble = 0;
    msgOFDMSymbols = IFFT(msgQAMSymbols); % ifft

    %% 拼接训练和信息序列
    OFDMSymbols = [repmat(preambleOFDMSymbols, PreambleNumber, 1); msgOFDMSymbols];
