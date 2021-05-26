function [OFDMSymbols] = IFFT(QAMSymbols)
    global IsPreamble
    global PreambleCarrierPositions
    global DataCarrierPositions
    global FFTSize
    global OFDMPositions
    global CPLength
    global SToPcol

    if IsPreamble == 1;
        symbolLength = 1;
        positionsIn = PreambleCarrierPositions;
        positionsOut = 1:FFTSize;
    else
        symbolLength = SToPcol;
        positionsIn = DataCarrierPositions;
        positionsOut = OFDMPositions;
    end

    %% ���任ǰ������
    ifftBlock = zeros(FFTSize, symbolLength); % Ԥ��padding
    ifftBlock(positionsIn, :) = QAMSymbols; % "����"QAM����
    ifftBlock(FFTSize + 2 - positionsIn, :) = conj(ifftBlock(positionsIn, :)); % "����"�乲��
    %% ���б任
    OFDMSymbols = ifft(ifftBlock); % ��׼ifft
    %% ����任�������
    OFDMSymbols = OFDMSymbols(1:length(positionsOut), :); % ��ȡ��Ҫ�����ز�
    OFDMSymbols = [OFDMSymbols(end - CPLength / 2 + 1:end, :); OFDMSymbols; OFDMSymbols(1:CPLength / 2, :)]; % ����ѭ��ǰ׺
    OFDMSymbols = reshape(OFDMSymbols, [], 1); % ��->��ת��
