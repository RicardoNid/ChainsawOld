function QAMSymbols = Qammod(bitAllocated, bits)
    global QAM8

    if bitAllocated == 3;
        qam8bit = reshape(bits, 3, [])';
        qam8dec = bi2de(qam8bit, 'left-msb');
        QAMSymbols = QAM8(qam8dec + 1);
        QAMSymbols = QAMSymbols';
    else
        % R2016a
        % M = 2^bitAllocated;
        % modObj = modem.qammod('M', M, 'SymbolOrder', 'Gray', 'InputType', 'Bit');
        % QAMSymbols = modulate(modObj, bits); % 3584 * 1 complex
        QAMSymbols = qammod(bits, 2^bitAllocated, 'gray', 'InputType', 'bit');
    end
