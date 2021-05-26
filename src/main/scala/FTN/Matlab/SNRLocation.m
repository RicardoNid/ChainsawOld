function SNR = SNRLocation(recoveredSymbols, transmittedSymbols)
    global SubcarriersNum
    SNR = zeros(SubcarriersNum, 1);

    for i = 1:SubcarriersNum
        SNR(i) = sum(abs(transmittedSymbols(i, :)).^2) / sum(abs(recoveredSymbols(i, :) - transmittedSymbols(i, :)).^2);
    end

end
