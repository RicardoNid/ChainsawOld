function none = Run()
    global CurrentFrame
    global FrameNum
    global On

    bitsAllFrame = []; % ���������ǰ
    debitsAllFrame = []; % �ռ����ս��

    CurrentFrame = 1;

    if On == 0
        TrueFrameNum = 1;
    else
        TrueFrameNum = FrameNum;
    end

    for cir = 1:TrueFrameNum
        %% �����
        msgBits = BitGen(); % ��֡����Ϣ����
        bitsAllFrame = [bitsAllFrame; msgBits]; % ��¼��Ϣ����
        OFDMFrame = OFDMFrameGenerator(msgBits); % ������ӹ�,�õ���֡,����
        %% �ŵ�
        OFDMFrame = filter([1, 0.8, 0.1, 0.05, 0.01, 0.005], 1, OFDMFrame);
        SNR = 12;
        snr = 10^(SNR / 10);
        code_power = norm(OFDMFrame)^2 / (length(OFDMFrame)); % �źŵķ��Ź��� =var(passchan_ofdm_symbol)
        sigma = sqrt(code_power / (snr * 2)); % sigma�뵱ǰSNR���ź�ƽ�������й�ϵ
        [OFDMFrame_rec, ~] = addnoise(OFDMFrame, sigma); % ֻ��ʵ���������
        %% ���ջ�
        receivedBits = OFDMFrameReceiver(OFDMFrame_rec); % ���ջ�������֡
        debitsAllFrame = [debitsAllFrame; receivedBits]; % ��¼��Ϣ����
        CurrentFrame = CurrentFrame + 1;
    end

    [nErrors_HD, ber_HD] = biterr(bitsAllFrame, debitsAllFrame); % �Ա���Ϣ����,����������
    display(nErrors_HD) % չʾ����������������,���ڱ�֤�����޸ĵİ�ȫ��
    display(ber_HD)
