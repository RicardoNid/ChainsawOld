function [decoded] = OFDMFrameReceiver(recvOFDMFrame)
    global On
    global IsPreamble
    global PowerOn
    global CPLength
    global DataCarrierPositions
    global PreambleNumber
    global SToPcol
    global FFTSize
    global RmsAlloc

    %% ���沿�ֶ�Ӧ��ͼ1
    recvPreambleOFDMSymbols = recvOFDMFrame(1:PreambleNumber * (FFTSize + CPLength)); % ���յ�����֡��Ϊѵ�����к���Ϣ����
    recvMsgOFDMSymbols = recvOFDMFrame(PreambleNumber * (FFTSize + CPLength) + 1:end);

    IsPreamble = 1;
    reambleQAMSymbols = FFT(recvPreambleOFDMSymbols);

    IsPreamble = 0;
    FDE = FFT(recvMsgOFDMSymbols); % fft,FDE�ߴ�224*16

    % �ŵ����ƺ;�?
    H = ChannelEstimation(reambleQAMSymbols); % �ŵ�����,�õ�(ѵ��������ռ�ݵ�)�������ز��ϵ�����ϵ��,H�ߴ�255*1

    % �ŵ���?
    for i = 1:SToPcol;
        FDE(:, i) = FDE(:, i) ./ H(DataCarrierPositions - 1); % ?? �˴������ز������������
    end

    %% ���沿�ֶ�Ӧ��ͼ2
    diffQAMSymbols = Iterating(FDE);

    %% ���沿�ֶ�Ӧ��ͼ3
    PowerOn = 0; % ȥ���ʷ���
    diffQAMSymbols = PowerOnOff(diffQAMSymbols);

    decoded = QAM2Bits(diffQAMSymbols); % QAM��ӳ�� -> �⽻֯ -> ά�ر�����

    if On == 0
        % ?? �˴�����Ҳ�ǲ���Ҫ��
        dataQAMSymbolsForAlloc = diffQAMSymbols * RmsAlloc(4);
        Alloc(dataQAMSymbolsForAlloc);
    end
