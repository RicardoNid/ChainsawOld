function diffQAMSymbols = Iterating(FDE)
    global IsPreamble
    global PowerOn
    global Iteration

    %% 关于FDE的命名我又做了仔细考虑,之前内外功率分配同时去除是对外部的QAM解映射产生了影响,而不是功率分配累积
    % Iterating内是局部作用域,FDE命名没有side effcet,因此改回FDE
    % 更进一步说,matlab没有按引用传递,只有按值传递,所以所有side effct只能通过返回值产生

    diffQAMSymbols = FDE;

    for iter = 1:Iteration

        PowerOn = 0; % 去功率分配
        diffQAMSymbols = PowerOnOff(diffQAMSymbols);

        recvBits = QAM2Bits(diffQAMSymbols); % QAM解映射 -> 解交织 -> 维特比译码

        %% 关于此部分代码的理解请参见图NO-DMT DSP
        QAMSymbols = Bits2QAM(recvBits); % 旁路1,QAMSymbols

        PowerOn = 1; % 加功率分配
        QAMSymbols = PowerOnOff(QAMSymbols);

        IsPreamble = 0;
        OFDMSymbols = IFFT(QAMSymbols);

        recvQAMSymbols = FFT(OFDMSymbols); % 旁路2,recvQAMSymbols

        %% 旁路汇集部分
        ICI = recvQAMSymbols - QAMSymbols;
        diffQAMSymbols = FDE - ICI;
    end
