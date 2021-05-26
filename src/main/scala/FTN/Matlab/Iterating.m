function diffQAMSymbols = Iterating(FDE)
    global IsPreamble
    global PowerOn
    global Iteration

    %% ����FDE����������������ϸ����,֮ǰ���⹦�ʷ���ͬʱȥ���Ƕ��ⲿ��QAM��ӳ�������Ӱ��,�����ǹ��ʷ����ۻ�
    % Iterating���Ǿֲ�������,FDE����û��side effcet,��˸Ļ�FDE
    % ����һ��˵,matlabû�а����ô���,ֻ�а�ֵ����,��������side effctֻ��ͨ������ֵ����

    diffQAMSymbols = FDE;

    for iter = 1:Iteration

        PowerOn = 0; % ȥ���ʷ���
        diffQAMSymbols = PowerOnOff(diffQAMSymbols);

        recvBits = QAM2Bits(diffQAMSymbols); % QAM��ӳ�� -> �⽻֯ -> ά�ر�����

        %% ���ڴ˲��ִ���������μ�ͼNO-DMT DSP
        QAMSymbols = Bits2QAM(recvBits); % ��·1,QAMSymbols

        PowerOn = 1; % �ӹ��ʷ���
        QAMSymbols = PowerOnOff(QAMSymbols);

        IsPreamble = 0;
        OFDMSymbols = IFFT(QAMSymbols);

        recvQAMSymbols = FFT(OFDMSymbols); % ��·2,recvQAMSymbols

        %% ��·�㼯����
        ICI = recvQAMSymbols - QAMSymbols;
        diffQAMSymbols = FDE - ICI;
    end
