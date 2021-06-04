function OFDMSymbols = OFDMFrameGenerator(msgBits)

    global On
    global IsPreamble
    global PowerOn
    global PreambleNumber

    %% ����ͨ·
    % ��ȡԤ�ȴ洢��ѵ������QAM����
    load './data/preambleQAMSymbols' preambleQAMSymbols

    if On == 0 % ѵ��ģʽʱ,��ȡԤ�ȴ洢��ѵ����֡QAM����
        % msgQAMSymbols = Bits2QAM(msgBits);
        load './data/msgQAMSymbols'
    else
        % ����ģʽʱ,����Ϣ���ؼӹ�ΪQAM����
        % msgQAMSymbols = Bits2QAM(msgBits); % ������� -> ��֯ -> QAMӳ��
        msgQAMSymbols = innerBits2QAM(msgBits); % ������� -> ��֯ -> QAMӳ��
    end

    PowerOn = 1; % �ӹ��ʷ���
    poweredMsgQAMSymbols = PowerOnOff(msgQAMSymbols);
    save './data/poweredMsgQAMSymbols' poweredMsgQAMSymbols;
    
    %% ��ѵ��QAM���żӹ�ΪOFDM����
    IsPreamble = 1;
    preambleOFDMSymbols = IFFT(preambleQAMSymbols);
    save './data/preambleOFDMSymbols' preambleOFDMSymbols;
    
    %% ����ϢQAM���żӹ�ΪOFDM����
    IsPreamble = 0;
    msgOFDMSymbols = IFFT(poweredMsgQAMSymbols); % ifft
    save './data/msgOFDMSymbols' msgOFDMSymbols;

    %% ƴ��ѵ������Ϣ����
    OFDMSymbols = [repmat(preambleOFDMSymbols, PreambleNumber, 1); msgOFDMSymbols];
    save './data/OFDMSymbols' OFDMSymbols;

function result = innerBits2QAM(msgBits)
    load('./data/bitAlloc.mat'); % ���ط���
    RmsAlloc = [1, sqrt(2), sqrt(3 + sqrt(3)), sqrt(10), sqrt(20), sqrt(42), sqrt(82), sqrt(170)];
    QAM8 = [-1 - sqrt(3), -1 + 1i, -1 - 1i, 1i * (1 + sqrt(3)), -1i * (1 + sqrt(3)), 1 + 1i, 1 - 1i, 1 + sqrt(3)];
    
    bits = transpose(msgBits); % 1 * 7168
    hardwareBits = reshape(bits, 448, 16);
    
    trellis = poly2trellis(7, [171,133]);
    convCodedBits = convenc(bits, trellis); % 1 * 14336
    hardwareConvCodedBits = reshape(convCodedBits, 896, 16);
    
    hardwareInterleavedBits = zeros(896, 16);
    for i = 1 : 16
        cycle = hardwareConvCodedBits(:, i);
        reshaped = reshape(cycle, 896 / 32, 32);
        rebuilt = reshape(transpose(reshaped), [], 1);
        hardwareInterleavedBits(:, i) = rebuilt;
    end
    
    % QAM
    hardwareModulatedSymbols = zeros(224, 16);
    segmentHead = 1;
    for i = 1 : length(bitAlloc)
        bitsForChannel = hardwareInterleavedBits(segmentHead:segmentHead + bitAlloc(i) - 1, :);
        segmentHead = segmentHead + bitAlloc(i);
        if bitAlloc(i) == 3
            % qam8bit = reshape(bitsForChannel, 3, [])';
            % qam8dec = bi2de(qam8bit, 'left-msb');
            % symbolsForChannel = QAM8(qam8dec + 1);
            % symbolsForChannel = symbolsForChannel';
            symbolsForChannel = QAMMOD8(bitsForChannel);
        else
            symbolsForChannel = qammod(bitsForChannel, 2^bitAlloc(i), 'gray', 'InputType', 'bit');
        end
        symbolsForChannel = symbolsForChannel / RmsAlloc(bitAlloc(i)); % ��һ��
        hardwareModulatedSymbols(i, :) = symbolsForChannel;
    end
    result = hardwareModulatedSymbols;

function result = QAMMOD8(bits) % ���ձ�����
    symbols = [-1 - sqrt(3), -1 + 1i, -1 - 1i, 1i * (1 + sqrt(3)), -1i * (1 + sqrt(3)), 1 + 1i, 1 - 1i, 1 + sqrt(3)];
    qam8bit = reshape(bits, 3, [])'; % bi2de�����ϲ���,�и�,��Ϊ����,�õ�����
    qam8dec = bi2de(qam8bit, 'left-msb'); % �����ϲ���,�õ�������
    result = symbols(qam8dec + 1)'; % indexing�õ�symbol��,����symbol��