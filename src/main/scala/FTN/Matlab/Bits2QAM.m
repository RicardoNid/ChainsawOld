function QAMSymbols = Bits2QAM(bits)
    convCodedBits = Convenc(bits); % �������
    interleavedBits = Interleave(convCodedBits); % ��֯
    QAMSymbols = DynamicQammod(interleavedBits); % (��̬)QAMӳ��
