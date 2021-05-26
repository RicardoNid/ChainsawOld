function none = Run()
    global CurrentFrame
    global FrameNum
    global On

    bitsAllFrame = []; % 发射机处理前
    debitsAllFrame = []; % 收集接收结果

    CurrentFrame = 1;

    if On == 0
        TrueFrameNum = 1;
    else
        TrueFrameNum = FrameNum;
    end

    for cir = 1:TrueFrameNum
        %% 发射机
        msgBits = BitGen(); % 子帧的信息比特
        bitsAllFrame = [bitsAllFrame; msgBits]; % 记录信息比特
        OFDMFrame = OFDMFrameGenerator(msgBits); % 发射机加工,得到子帧,发出
        %% 信道
        OFDMFrame = filter([1, 0.8, 0.1, 0.05, 0.01, 0.005], 1, OFDMFrame);
        SNR = 12;
        snr = 10^(SNR / 10);
        code_power = norm(OFDMFrame)^2 / (length(OFDMFrame)); % 信号的符号功率 =var(passchan_ofdm_symbol)
        sigma = sqrt(code_power / (snr * 2)); % sigma与当前SNR和信号平均能量有关系
        [OFDMFrame_rec, ~] = addnoise(OFDMFrame, sigma); % 只在实部添加噪声
        %% 接收机
        receivedBits = OFDMFrameReceiver(OFDMFrame_rec); % 接收机接收子帧
        debitsAllFrame = [debitsAllFrame; receivedBits]; % 记录信息比特
        CurrentFrame = CurrentFrame + 1;
    end

    [nErrors_HD, ber_HD] = biterr(bitsAllFrame, debitsAllFrame); % 对比信息比特,计算误码率
    display(nErrors_HD) % 展示误码数量和误码率,用于保证代码修改的安全性
    display(ber_HD)
