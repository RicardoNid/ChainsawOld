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
    msgQAMSymbols = PowerOnOff(msgQAMSymbols);

    %% ��ѵ��QAM���żӹ�ΪOFDM����
    IsPreamble = 1;
    preambleOFDMSymbols = IFFT(preambleQAMSymbols);

    %% ����ϢQAM���żӹ�ΪOFDM����
    IsPreamble = 0;
    msgOFDMSymbols = IFFT(msgQAMSymbols); % ifft

    %% ƴ��ѵ������Ϣ����
    OFDMSymbols = [repmat(preambleOFDMSymbols, PreambleNumber, 1); msgOFDMSymbols];
