function none = Alloc(recoveredSymbols)

    % 初始化迭代参数,其中gap是chow算法的目标,作为参数传入;其它参数为其调整服务
    global BER
    global SER
    global Gap
    global Miu
    global RmsAlloc
    global BitsPerSymbolQAM

    %% 比特分配的依据是最后一次迭代产生的差值QAM符号和真实QAM符号
    % 实际实现中,比特分配采用固定的子帧,对应的QAM符号,同时存储在接收机与发射机

    load './data/msgQAMSymbols'
    msgQAMSymbols = msgQAMSymbols * RmsAlloc(BitsPerSymbolQAM);
    SNR = SNRLocation(recoveredSymbols, msgQAMSymbols); % SNR是对比真实QAM符号和迭代后恢复的QAM符号得到

    % 初次调用
    [bits_allo, power_allo, total_bits] = chow_algo_all(SNR, Gap);
    display('first time done')

    % 因为上面设置的gap由设定的BER得来，但不一定是最优的，通过下面的方式，找到该SNR所能支持的最优的gap
    % 没有任何比特被分配时,往flag = 1的方向调整目标
    % ?? 当前逻辑中,使用的比特/功率分配是chow算法倒数第二次运行的结果,这是正确的行为吗?
    if total_bits == 0
        display('mode 1')
        flag = 1;

        while (BER > 0) && (BER < 0.2) && (total_bits == 0)
            BER = BER + flag * Miu;
            SER = 1 - (1 - BER)^4;
            Gap = 1/3 * ((SER / 4))^2;
            bits_alloc_record = bits_allo;
            power_alloc_record = power_allo;
            [bits_allo, power_allo, total_bits] = chow_algo_all(SNR, Gap);
        end

    end

    % 有比特被分配时,往flag = 1的方向调整目标
    if total_bits ~= 0
        display('mode 2')
        flag = -1;

        while (BER > 0) && (BER < 0.2) && (total_bits > 0)
            BER = BER + flag * Miu;
            SER = 1 - (1 - BER)^4;
            Gap = 1/3 * (qfuncinv(SER / 4))^2;
            bits_alloc_record = bits_allo;
            power_alloc_record = power_allo;
            [bits_allo, power_allo, total_bits] = chow_algo_all(SNR, Gap);
        end

    end

    bits_alloc = bits_alloc_record;
    powerAlloc = power_alloc_record';

    [bitAllocSort, bitAllocSum] = bits_alloc_position_sum(bits_alloc');
    bitAlloc = bits_alloc;
    save './data/bitAlloc' bitAlloc
    save './data/bitAllocSort' bitAllocSort;
    save './data/bitAllocSum' bitAllocSum;
    save './data/powerAlloc' powerAlloc;
