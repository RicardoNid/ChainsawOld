function [H] = ChannelEstimation(recvPreambleQAMSymbols)
    global PreambleNumber
    global PreambleCarriersNum
    global HTap

    load './data/preambleQAMSymbols' % 接收机内置preambleQAMSymbol的QAM符号序列

    %% 自此以下的部分需要硬件实现
    ratio = zeros(PreambleCarriersNum, PreambleNumber);

    for i = 1:PreambleNumber
        % 计算求得preamble符号和实际preamble符号的比值作为信道估计
        ratio(:, i) = recvPreambleQAMSymbols(:, i) ./ preambleQAMSymbols;
    end

    H = mean(ratio, 2); % 对两个比值序列求均值得到信道估计
    H = smooth(H, HTap); % 对修正系数做span为20的滑动平均
