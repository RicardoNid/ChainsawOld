function bits = Qamdemod(bitAllocated, QAMSymbols)
    global RmsAlloc
    global QAM8

    if bitAllocated == 3;

        QAMSymbols = QAMSymbols * RmsAlloc(3);
        QAMSymbols = QAMSymbols';
        [~, index] = min(abs(repmat(QAMSymbols, 8, 1) - repmat(transpose(QAM8), 1, length(QAMSymbols))));
        temp = de2bi(index - 1, 3, 'left-msb');
        bits = reshape(temp', 1, []);

    else
        QAMSymbols = QAMSymbols / rms(QAMSymbols) * RmsAlloc(bitAllocated);
        % QAMSymbols = QAMSymbols * RmsAlloc(bitAllocated);

        % R2016a
        % M = 2^bitAllocated;
        % modObj = modem.qammod('M', M, 'SymbolOrder', 'Gray', 'InputType', 'Bit');
        % demodObj = modem.qamdemod(modObj);
        % set(demodObj, 'DecisionType', 'Hard decision');
        % bits = demodulate(demodObj, QAMSymbols);

        bits = qamdemod(QAMSymbols, 2^bitAllocated, 'gray', 'OutputType', 'bit');
        bits = bits';
    end
