function OFDMSymbols = FFT(OFDMSignals)
    global IsPreamble
    global OFDMPositions
    global PreambleNumber
    global PreambleCarrierPositions
    global DataCarrierPositions
    global FFTSize
    global CPLength
    global SToPcol

    if IsPreamble == 1;
        symbolLength = PreambleNumber;
        positionsOut = PreambleCarrierPositions;
        positionsIn = 1:FFTSize;
    else
        symbolLength = SToPcol;
        positionsOut = DataCarrierPositions;
        positionsIn = OFDMPositions;
    end

    %% ����任ǰ������
    OFDMSignals = reshape(OFDMSignals, [], symbolLength); % ��->��ת��
    OFDMSignals = OFDMSignals(CPLength / 2 + 1:end - CPLength / 2, :); % ȥ��ѭ��ǰ׺
    %% ���任ǰ������
    fftBlock = zeros(FFTSize, symbolLength);
    fftBlock(1:length(positionsIn), :) = OFDMSignals;
    %% ���б任
    OFDMSignals = fft(fftBlock); % ��׼fft
    %% ��ȡ�任�������
    OFDMSymbols = OFDMSignals(positionsOut, :); % ��ȡ��Ҫ�����ز�
