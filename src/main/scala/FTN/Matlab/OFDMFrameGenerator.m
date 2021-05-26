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
        msgQAMSymbols = Bits2QAM(msgBits); % ������� -> ��֯ -> QAMӳ��
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