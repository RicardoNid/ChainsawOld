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
        % msgQAMSymbols = Bits2QAM(msgBits); % 卷积编码 -> 交织 -> QAM映射
        msgQAMSymbols = innerBits2QAM(msgBits); % 卷积编码 -> 交织 -> QAM映射
    end

    PowerOn = 1; % 加功率分配
    poweredMsgQAMSymbols = PowerOnOff(msgQAMSymbols);
    save './data/poweredMsgQAMSymbols' poweredMsgQAMSymbols;
    
    %% 将训练QAM符号加工为OFDM符号
    IsPreamble = 1;
    preambleOFDMSymbols = IFFT(preambleQAMSymbols);
    save './data/preambleOFDMSymbols' preambleOFDMSymbols;
    
    %% 将信息QAM符号加工为OFDM符号
    IsPreamble = 0;
    msgOFDMSymbols = IFFT(poweredMsgQAMSymbols); % ifft
    save './data/msgOFDMSymbols' msgOFDMSymbols;

    %% 拼接训练和信息序列
    OFDMSymbols = [repmat(preambleOFDMSymbols, PreambleNumber, 1); msgOFDMSymbols];
    save './data/OFDMSymbols' OFDMSymbols;

function result = innerBits2QAM(msgBits)
    load('./data/bitAlloc.mat'); % 比特分配
    RmsAlloc = [1, sqrt(2), sqrt(3 + sqrt(3)), sqrt(10), sqrt(20), sqrt(42), sqrt(82), sqrt(170)];
    QAM8 = [-1 - sqrt(3), -1 + 1i, -1 - 1i, 1i * (1 + sqrt(3)), -1i * (1 + sqrt(3)), 1 + 1i, 1 - 1i, 1 + sqrt(3)];
    
    bits = transpose(msgBits); % 1 * 7168
    hardwareBits = reshape(bits, 448, 16);
    
    trellis = poly2trellis(7, [171,133]);
    convCodedBits = convenc(bits, trellis); % 1 * 14336
    hardwareConvCodedBits = reshape(convCodedBits, 896, 16);
    
    hardwareInterleavedBits = zeros(896, 16);
    for i = 1 : 16
        cycle = hardwareConvCodedBits(:, i);
        reshaped = reshape(cycle, 896 / 32, 32);
        rebuilt = reshape(transpose(reshaped), [], 1);
        hardwareInterleavedBits(:, i) = rebuilt;
    end
    
    % QAM
    hardwareModulatedSymbols = zeros(224, 16);
    segmentHead = 1;
    for i = 1 : length(bitAlloc)
        bitsForChannel = hardwareInterleavedBits(segmentHead:segmentHead + bitAlloc(i) - 1, :);
        segmentHead = segmentHead + bitAlloc(i);
        if bitAlloc(i) == 3
            % qam8bit = reshape(bitsForChannel, 3, [])';
            % qam8dec = bi2de(qam8bit, 'left-msb');
            % symbolsForChannel = QAM8(qam8dec + 1);
            % symbolsForChannel = symbolsForChannel';
            symbolsForChannel = QAMMOD8(bitsForChannel);
        else
            symbolsForChannel = qammod(bitsForChannel, 2^bitAlloc(i), 'gray', 'InputType', 'bit');
        end
        symbolsForChannel = symbolsForChannel / RmsAlloc(bitAlloc(i)); % 归一化
        hardwareModulatedSymbols(i, :) = symbolsForChannel;
    end
    result = hardwareModulatedSymbols;

function result = QAMMOD8(bits) % 接收比特行
    symbols = [-1 - sqrt(3), -1 + 1i, -1 - 1i, 1i * (1 + sqrt(3)), -1i * (1 + sqrt(3)), 1 + 1i, 1 - 1i, 1 + sqrt(3)];
    qam8bit = reshape(bits, 3, [])'; % bi2de在行上操作,切割,分为多列,得到多行
    qam8dec = bi2de(qam8bit, 'left-msb'); % 在行上操作,得到索引列
    result = symbols(qam8dec + 1)'; % indexing得到symbol列,返回symbol行