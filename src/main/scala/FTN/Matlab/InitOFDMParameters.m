function OFDMParameters = InitOFDMParameters()

    %% ���Ʊ���,�����пɱ�,ͨ���ı����������Ʒ�������
    % on = 0 ѵ��(�������/���ʷ���)ģʽ��on = 1 ����(���ر���/���ʷ���)ģʽ
    global On; On = 0;
    % ��ǰ��֡,���������ȡ��/�Ƿ���б��ط���������
    global CurrentFrame; CurrentFrame = 1;
    % ָʾ��ǰ�ǽ��й��ʼ��ػ��ǹ���ȥ��,��Ϊmatlab������ö��,���Ա��𴫲�,Ҫ��ÿ���޸�ȫ�ֱ�����Ϊ����
    global PowerOn; PowerOn = 1;
    % ָʾ��ǰFFT/IFFT�������ѵ�����л�����Ч����,ʹ��ȫ�ֱ�����ԭ��ͬ��
    global IsPreamble; IsPreamble = 1;
    % global DoInterleave; DoInterleave = 1;

    %% ����,�����в��ɱ�
    %% OFDM����
    % ѭ��ǰ׺����
    global CPLength; CPLength = 20;
    % ѵ�������ظ�����
    global PreambleNumber; PreambleNumber = 2;
    % fft�ߴ�
    global FFTSize; FFTSize = 512;

    %% �������-ά�ر��������
    % Լ���ֳ�
    ConvConstLen = 7;
    ConvCodeGen = [171, 133];
    global trellis; trellis = poly2trellis (ConvConstLen, ConvCodeGen);
    % ά�ر�����������������
    global tblen; tblen = 90;
    % ������
    global ConvCodeRate; ConvCodeRate = 1/2;

    %% OFDMϵͳ�������
    % Symbol
    global OFDMSymbolNumber; OFDMSymbolNumber = 8;
    global BitsPerSymbolQAM; BitsPerSymbolQAM = 4;
    global PreambleBitsPerSymbolQAM; PreambleBitsPerSymbolQAM = 4;
    global SToPcol; SToPcol = OFDMSymbolNumber / ConvCodeRate;
    % ���ز�
    global DataCarrierPositions; DataCarrierPositions = 3:226;
    global PreambleCarrierPositions; PreambleCarrierPositions = 2:FFTSize / 2;
    global SubcarriersNum; SubcarriersNum = length(DataCarrierPositions);
    global PreambleCarriersNum; PreambleCarriersNum = length(PreambleCarrierPositions);
    global OFDMPositions; OFDMPositions = sort([1 DataCarrierPositions FFTSize / 2 + 1 FFTSize + 2 - DataCarrierPositions]);
    % ֡����
    global BitNumber; BitNumber = length(DataCarrierPositions) * OFDMSymbolNumber * BitsPerSymbolQAM;
    global PreambleBitNumber; PreambleBitNumber = length(PreambleCarrierPositions) * PreambleBitsPerSymbolQAM;
    % ��������
    global Iteration; Iteration = 5;

    %% ��֯����
    % ��֯���
    global InterleaverDepth; InterleaverDepth = 32;

    %% QAM����
    % QAM8�ķ���
    global QAM8; QAM8 = [-1 - sqrt(3), -1 + 1i, -1 - 1i, 1i * (1 + sqrt(3)), -1i * (1 + sqrt(3)), 1 + 1i, 1 - 1i, 1 + sqrt(3)];
    % ��ͬ����������,QAM����rms������ֵ
    global RmsAlloc; RmsAlloc = [1, sqrt(2), sqrt(3 + sqrt(3)), sqrt(10), sqrt(20), sqrt(42), sqrt(82), sqrt(170)];

    %% �ŵ��������
    % ����ϵ���Ļ���ƽ����ͷ��
    global HTap; HTap = 20;

    %% ���Թ�ģ����������Ӳ���
    global PreambleSeed; PreambleSeed = 20;
    global Seed; Seed = [30, 13, 21, 20, 8, 9, 15, 17, 19, 12, 11, 30, 25, 27, 26, 22, 14, 7, 23, 29];
    % global Seed; Seed = randi(30, [1, 20]);
    % global FrameNum; FrameNum = 1;
    global FrameNum; FrameNum = 20;
    % global Seed; Seed = randi(100, [1, 100]);
    % global FrameNum; FrameNum = 100;

    %% Chow�㷨��ر���,�����пɱ�
    global BER; BER = 1E-3;
    global SER; SER = 1 - (1 - BER)^4;
    global Gap; Gap = 1/3 * (qfuncinv(SER / 4))^2;
    global TargetBits; TargetBits = SubcarriersNum * BitsPerSymbolQAM;
    % BER�����ĵ�������
    global Miu; Miu = 1e-5;
