function OFDMParameters = InitOFDMParameters()

    %% 控制变量,过程中可变,通过改变它们来控制仿真流程
    % on = 0 训练(计算比特/功率分配)模式，on = 1 工作(加载比特/功率分配)模式
    global On; On = 0;
    % 当前子帧,和随机种子取用/是否进行比特分配计算相关
    global CurrentFrame; CurrentFrame = 1;
    % 指示当前是进行功率加载还是功率去除,因为matlab不方便枚举,所以比起传参,要求每次修改全局变量更为明显
    global PowerOn; PowerOn = 1;
    % 指示当前FFT/IFFT处理的是训练序列还是有效数据,使用全局变量的原因同上
    global IsPreamble; IsPreamble = 1;
    % global DoInterleave; DoInterleave = 1;

    %% 参数,过程中不可变
    %% OFDM参数
    % 循环前缀长度
    global CPLength; CPLength = 20;
    % 训练序列重复次数
    global PreambleNumber; PreambleNumber = 2;
    % fft尺寸
    global FFTSize; FFTSize = 512;

    %% 卷积编码-维特比译码参数
    % 约束字长
    ConvConstLen = 7;
    ConvCodeGen = [171, 133];
    global trellis; trellis = poly2trellis (ConvConstLen, ConvCodeGen);
    % 维特比译码抛弃比特数量
    global tblen; tblen = 90;
    % 编码率
    global ConvCodeRate; ConvCodeRate = 1/2;

    %% OFDM系统整体参数
    % Symbol
    global OFDMSymbolNumber; OFDMSymbolNumber = 8;
    global BitsPerSymbolQAM; BitsPerSymbolQAM = 4;
    global PreambleBitsPerSymbolQAM; PreambleBitsPerSymbolQAM = 4;
    global SToPcol; SToPcol = OFDMSymbolNumber / ConvCodeRate;
    % 子载波
    global DataCarrierPositions; DataCarrierPositions = 3:226;
    global PreambleCarrierPositions; PreambleCarrierPositions = 2:FFTSize / 2;
    global SubcarriersNum; SubcarriersNum = length(DataCarrierPositions);
    global PreambleCarriersNum; PreambleCarriersNum = length(PreambleCarrierPositions);
    global OFDMPositions; OFDMPositions = sort([1 DataCarrierPositions FFTSize / 2 + 1 FFTSize + 2 - DataCarrierPositions]);
    % 帧长度
    global BitNumber; BitNumber = length(DataCarrierPositions) * OFDMSymbolNumber * BitsPerSymbolQAM;
    global PreambleBitNumber; PreambleBitNumber = length(PreambleCarrierPositions) * PreambleBitsPerSymbolQAM;
    % 迭代次数
    global Iteration; Iteration = 5;

    %% 交织参数
    % 交织深度
    global InterleaverDepth; InterleaverDepth = 32;

    %% QAM参数
    % QAM8的符号
    global QAM8; QAM8 = [-1 - sqrt(3), -1 + 1i, -1 - 1i, 1i * (1 + sqrt(3)), -1i * (1 + sqrt(3)), 1 + 1i, 1 - 1i, 1 + sqrt(3)];
    % 不同比特数量下,QAM符号rms的理论值
    global RmsAlloc; RmsAlloc = [1, sqrt(2), sqrt(3 + sqrt(3)), sqrt(10), sqrt(20), sqrt(42), sqrt(82), sqrt(170)];

    %% 信道均衡参数
    % 均衡系数的滑动平均抽头数
    global HTap; HTap = 20;

    %% 测试规模和随机数种子参数
    global PreambleSeed; PreambleSeed = 20;
    global Seed; Seed = [30, 13, 21, 20, 8, 9, 15, 17, 19, 12, 11, 30, 25, 27, 26, 22, 14, 7, 23, 29];
    % global Seed; Seed = randi(30, [1, 20]);
    % global FrameNum; FrameNum = 1;
    global FrameNum; FrameNum = 20;
    % global Seed; Seed = randi(100, [1, 100]);
    % global FrameNum; FrameNum = 100;

    %% Chow算法相关变量,过程中可变
    global BER; BER = 1E-3;
    global SER; SER = 1 - (1 - BER)^4;
    global Gap; Gap = 1/3 * (qfuncinv(SER / 4))^2;
    global TargetBits; TargetBits = SubcarriersNum * BitsPerSymbolQAM;
    % BER调整的迭代步长
    global Miu; Miu = 1e-5;
