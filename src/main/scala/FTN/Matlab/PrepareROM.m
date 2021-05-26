function none = PrepareROM()

    global RmsAlloc
    global PreambleBitsPerSymbolQAM
    global PreambleSeed
    global PreambleBitNumber
    global SubcarriersNum

    %% 准备分配数据
    % 未训练时的比特分配,所有子载波相同
    bitAlloc = ones(SubcarriersNum, 1) * PreambleBitsPerSymbolQAM;
    [bitAllocSort, bitAllocSum] = bits_alloc_position_sum(bitAlloc');
    % 未训练时的功率分配,所有子载波相同
    powerAlloc = ones(1, SubcarriersNum);
    % 未训练时RAM中的分配,训练后会被覆盖
    save './data/powerAlloc' powerAlloc;
    save './data/bitAlloc' bitAlloc
    save './data/bitAllocSort' bitAllocSort;
    save './data/bitAllocSum' bitAllocSum;

    %% 将训练比特加工为QAM符号
    % preambleBits = randint(PreambleBitNumber, 1, 2, PreambleSeed);
    rng(PreambleSeed)
    preambleBits = randi(2, PreambleBitNumber, 1) - 1;
    preambleQAMSymbols = qammod(preambleBits, 2^PreambleBitsPerSymbolQAM, 'gray', 'InputType', 'bit');
    preambleQAMSymbols = preambleQAMSymbols / RmsAlloc(4);
    % 实际实现时,发射/接收机都从ROM中读取预先存储的训练序列QAM符号,实验中,以文件存取形式模拟
    save './data/preambleQAMSymbols' preambleQAMSymbols

    msgBits = BitGen(); % 子帧的信息比特
    %% 将信息比特加工为QAM符号
    msgQAMSymbols = Bits2QAM(msgBits); % 卷积编码 -> 交织 -> QAM映射
    % 实际实现时,比特分配时,发射/接收机都从ROM中读取预先存储的训练子帧的QAM符号,实验中,以文件存取形式模拟
    save './data/msgQAMSymbols' msgQAMSymbols;
