function none = PrintQamSymbols()

    clc
    clear

    idealRMS = [];

    for bitAllocated = 5:5

        % for i = 0:M - 1
        %     bits = [bits, de2bi(i)];
        % end

        bits = [];
        bits = de2bi(0:M - 1);
        bits = bits';

        % R2016a
        % M = 2^bitAllocated;
        % modObj = modem.qammod('M', M, 'SymbolOrder', 'Gray', 'InputType', 'Bit');
        % QAMSymbols = modulate(modObj, bits)

        QAMSymbols = qammod(bits, 2^bitAllocated, 'gray', 'InputType', 'bit');
        idealRMS = [idealRMS, rms(QAMSymbols)];
    end

    idealRMS
